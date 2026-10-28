"""Optional punctuation remains source text and introduces no ternary."""

import pytest

from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcognitive import LizardExtension as Cognitive
from lizard_ext.lizardcomplextags import LizardExtension as ComplexTags
from lizard_ext.lizardhalstead import LizardExtension as Halstead
from lizard_ext.lizardignoreassert import LizardExtension as IgnoreAssert
from lizard_ext.lizardnd import LizardExtension as NestingDepth
from lizard_ext.lizardwordcount import LizardExtension as WordCount


def analyze(source, suffix, *extensions):
    return FileAnalyzer(get_extensions(list(extensions))).analyze_source_code(
        'example.' + suffix, source)


@pytest.mark.parametrize('separator', [' ', '/* comment */', '/* a /* b */ c */'])
def test_swift_comment_is_ternary_whitespace(separator):
    source = 'func f(_ c: Bool) -> Int { return c' + separator + '? 1 : 2 }'
    result = analyze(source, 'swift', Cognitive())
    function, = result.function_list
    assert (function.cyclomatic_complexity, function.cognitive_complexity) == (2, 1)


def test_swift_optional_return_and_extensions_preserve_question_mark():
    result = analyze('func f() -> (Int, Int)? { return nil }', 'swift',
                     Cognitive(), Halstead(), WordCount())
    function, = result.function_list
    assert (function.cyclomatic_complexity, function.cognitive_complexity) == (1, 0)
    assert function._halstead_operators['?'] == 1
    assert 'postfix?' not in function._halstead_operands
    assert 'postfix?' not in result.wordCount


def test_swift_optional_tuple_parameter_keeps_source_signature():
    result = analyze('func f(_ p: (Int, Int)?) {}', 'swift')
    function, = result.function_list
    assert function.long_name == 'f _ p : Int , Int ?'
    assert function.cyclomatic_complexity == 1


def test_swift_real_identifier_does_not_collide_with_optional_marker():
    result = analyze('struct postfix {}\nfunc f(_ p: postfix?) {}', 'swift')
    function, = result.function_list
    assert function.long_name == 'f _ p : postfix?'


def test_swift_only_ternaries_have_complexity_tags():
    result = analyze('func f() { return g()?.x }\n'
                     'func h(_ c: Bool) { return c/* comment */? 1 : 2 }',
                     'swift', ComplexTags())
    assert [f.complex_tags for f in result.function_list] == [[], [['?', 2]]]


def test_swift_only_ternaries_increase_nesting_depth():
    result = analyze('func f() { return g()?.x }\n'
                     'func h(_ c: Bool) { return c/* comment */? 1 : 2 }',
                     'swift', NestingDepth())
    assert [f.max_nesting_depth for f in result.function_list] == [0, 1]


@pytest.mark.parametrize('suffix', ['js', 'ts', 'tsx'])
def test_optional_access_and_nullish_preserve_function_boundaries(suffix):
    result = analyze('function f(a) {\n'
                     '  const x = a?.items?.[0]?.(v => v?.value);\n'
                     '  return x ?? (a ? 1 : 2);\n'
                     '}\nfunction later() { return 1; }', suffix)
    assert [(f.name, f.cyclomatic_complexity) for f in result.function_list] == [
        ('(anonymous)', 1), ('f', 3), ('later', 1)]


@pytest.mark.parametrize('suffix', ['js', 'ts', 'tsx'])
def test_optional_punctuation_is_not_a_word(suffix):
    result = analyze('function f(a) { return a()?.x ?? 0; }', suffix, WordCount())
    assert '?.' not in result.wordCount
    assert '??' not in result.wordCount
    assert result.wordCount['x'] == 1


@pytest.mark.parametrize('suffix', ['js', 'ts', 'tsx'])
def test_assertion_ignores_nullish_decision(suffix):
    result = analyze('function f(a, b) { assert(a ?? b); }', suffix, IgnoreAssert())
    assert [f.cyclomatic_complexity for f in result.function_list] == [1]


@pytest.mark.parametrize('suffix', ['js', 'ts', 'tsx'])
@pytest.mark.parametrize('expression', ['a ?? b', 'a??b', 'a ?? b ?? c'])
def test_nullish_coalescing_counts_one_decision_per_operator(suffix, expression):
    result = analyze('function f(a, b, c) { return ' + expression + '; }', suffix)
    expected = 3 if expression == 'a ?? b ?? c' else 2
    assert [f.cyclomatic_complexity for f in result.function_list] == [expected]


def test_swift_assertion_does_not_subtract_uncounted_optionals():
    result = analyze('func f() { assert(g()?.x ?? false) }', 'swift', IgnoreAssert())
    assert [f.cyclomatic_complexity for f in result.function_list] == [1]


def test_typescript_optional_parameter_preserves_name_and_count():
    result = analyze('function f(value?: number) { return value; }', 'ts')
    function, = result.function_list
    assert (function.name, function.parameter_count,
            function.cyclomatic_complexity) == ('f', 1, 1)


@pytest.mark.parametrize('suffix', ['js', 'ts', 'tsx'])
def test_decimal_ternary_is_not_optional_access(suffix):
    result = analyze('function f(a) { return a?.5:1; }', suffix)
    assert [f.cyclomatic_complexity for f in result.function_list] == [2]


def test_tsx_optional_operators_in_jsx():
    result = analyze('const C = (p) => {\n'
                     '  const n = p.a?.b ?? 0;\n'
                     '  return n > 1 ? <A/> : <B x={p.f()?.g}/>;\n'
                     '};\n', 'tsx')
    assert [f.cyclomatic_complexity for f in result.function_list] == [3]


@pytest.mark.parametrize('suffix', ['js', 'ts', 'jsx', 'tsx'])
def test_ternary_without_spaces_is_a_decision(suffix):
    result = analyze('function f(x, y) { return x?y:0; }\n'
                     'function g(x) { return x?1:x?.y?2:3; }', suffix)
    assert [f.cyclomatic_complexity for f in result.function_list] == [2, 3]


@pytest.mark.parametrize('suffix', ['ts', 'tsx'])
def test_optional_member_and_parameter_are_not_ternaries(suffix):
    result = analyze('class A {\n'
                     '  name?: string;\n'
                     '  m(a?: number, b?: string): number { return 1; }\n'
                     '}\n'
                     'function f(cb: (a?: number, b?) => void) { return 1; }\n',
                     suffix)
    assert [(f.name, f.cyclomatic_complexity, f.parameter_count)
            for f in result.function_list] == [('m', 1, 2), ('f', 1, 1)]
