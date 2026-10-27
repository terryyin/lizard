"""Public function boundaries for ternary branch arrows and internal colons."""

import pytest

from lizard import analyze_file


def summaries(filename, source):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in analyze_file.analyze_source_code(
                filename, source).function_list]


def language_source(extension, code):
    if extension == 'vue':
        return '<script lang="ts">\n' + code + '</script>\n', 1
    return code, 0


BRANCHES = [
    ('nested_ternary', 'c ? d ? x => x : y => y && d : z => z || c',
     [('f', 1), ('(anonymous)', 2), ('(anonymous)', 2)], 3),
    ('curried_branch', 'c ? x => y => y && x : z => z || c',
     [('f', 2), ('(anonymous)', 2)], 2),
    ('curried_internal_colon',
     'c ? x => d ? y => y : z => z || x : w => w',
     [('f', 3), ('(anonymous)', 1)], 2),
    ('grouped_nested_arrow', 'c ? x => (y => y && x) : z => z || c',
     [('(anonymous)', 2), ('f', 1), ('(anonymous)', 2)], 2),
    ('parenthesized_object', 'c ? x => ({a: x || d}) : y => y && d',
     [('f', 2), ('(anonymous)', 2)], 2),
    ('internal_ternary', 'x => c ? x : d', [('f', 2)], 1),
    ('curried_control', 'x => d ? y => y : z => z || x', [('f', 3)], 1),
    ('block_branches', 'c ? x => { return x; } : y => { return y || d; }',
     [('f', 1), ('(anonymous)', 2)], 2),
    ('plain_ternary', 'c ? d : c || d', [], 3),
]


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx', 'cjs', 'mjs', 'vue'])
@pytest.mark.parametrize('_label,expression,arrows,outer_ccn', BRANCHES,
                         ids=[case[0] for case in BRANCHES])
def test_branch_colons_and_internal_colons(extension, _label, expression,
                                          arrows, outer_ccn):
    code = (
        'function outer(c, d) {\n'
        f'  const f = {expression};\n'
        '  return 1;\n'
        '}\n'
        'function after() { return 1; }\n'
    )
    code, offset = language_source(extension, code)
    expected = [(name, 2 + offset, 2 + offset, ccn) for name, ccn in arrows]
    expected += [('outer', 1 + offset, 4 + offset, outer_ccn),
                 ('after', 5 + offset, 5 + offset, 1)]
    assert summaries('a.' + extension, code) == expected


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx', 'vue'])
def test_reported_two_parenthesized_branches(extension):
    code = (
        'function choose(strict, b) {\n'
        '  const eq = strict ? (x, y) => x === y : (x, y) => x == y || b;\n'
        '  return eq;\n'
        '}\n'
    )
    code, offset = language_source(extension, code)
    assert summaries('a.' + extension, code) == [
        ('(anonymous)', 2 + offset, 2 + offset, 1),
        ('(anonymous)', 2 + offset, 2 + offset, 2),
        ('choose', 1 + offset, 4 + offset, 2),
    ]


@pytest.mark.parametrize('extension', ['jsx', 'tsx'])
def test_jsx_attribute_contains_separate_branch_arrows(extension):
    code = (
        'function view(c, d) {\n'
        ' return <Panel f={c ? x => x : y => y || d}/>;\n'
        '}\n'
    )
    assert summaries('a.' + extension, code) == [
        ('(anonymous)', 2, 2, 1), ('(anonymous)', 2, 2, 2), ('view', 1, 3, 2),
    ]


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx'])
def test_property_colon_and_computed_key_keep_their_ownership(extension):
    code = (
        'function outer(c, d) {\n'
        '  const o = {f: c ? x => x : y => y || d, next: z => z && c};\n'
        '  return 1;\n'
        '}\n'
        'function after() { return 1; }\n'
    )
    functions = summaries('a.' + extension, code)
    assert functions[0] == ('f', 2, 2, 1)
    assert functions[-2:] == [('outer', 1, 4, 2), ('after', 5, 5, 1)]

    computed = code.replace(
        'f: c ? x => x : y => y || d', '[c ? "a" : "b"]: x => x || d')
    assert summaries('a.' + extension, computed) == [
        ('c?a:b', 2, 2, 2), ('next', 2, 2, 2),
        ('outer', 1, 4, 2), ('after', 5, 5, 1),
    ]


@pytest.mark.parametrize('extension', ['ts', 'tsx', 'vue'])
@pytest.mark.parametrize('expression,ccns,outer_ccn', [
    ('c ? (x: number): number => x : (y: number): number => y || d', [1, 2], 2),
    ('c ? (x): number => x : (y): number => y || d', [1, 2], 2),
    ('c ? (): number => 1 : (): number => 2 || d', [1, 2], 2),
    ('c ? (x: number): {a: number} => ({a: x}) : y => y || d', [1, 2], 2),
    ('c ? (x): {a: number} => ({a: x}) : y => y || d', [1, 2], 2),
    ('c ? (x: number) => x as number : (y: number) => y || d', [1, 2], 2),
    ('c ? d ? (x): number => x : (y): number => y : (z): number => z || c',
     [1, 1, 2], 3),
])
def test_typed_branch_annotations_are_not_runtime_colons(extension, expression,
                                                        ccns, outer_ccn):
    code = (
        'function outer(c, d) {\n'
        f'  const f = {expression};\n'
        '  return 1;\n'
        '}\n'
        'function after() { return 1; }\n'
    )
    code, offset = language_source(extension, code)
    expected = [('(anonymous)', 2 + offset, 2 + offset, ccn) for ccn in ccns]
    expected += [('outer', 1 + offset, 4 + offset, outer_ccn),
                 ('after', 5 + offset, 5 + offset, 1)]
    assert summaries('a.' + extension, code) == expected


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx'])
@pytest.mark.parametrize('alternate', ['y => y || d', '(y) => y || d'])
@pytest.mark.parametrize('value,outer_ccn', [
    ('d', 2), ('x = d ? a : b', 3), ('x = {a: d}', 2),
    ('x = call({a: d})', 2), ('x = (z => ({a: z}))(d)', 2),
])
def test_parenthesized_runtime_value_keeps_its_branch_separator(extension,
                                                               alternate, value,
                                                               outer_ccn):
    code = (
        'function outer(c, d) {\n'
        f'  const f = c ? ({value}) : {alternate};\n'
        '  return 1;\n'
        '}\n'
        'function after() { return 1; }\n'
    )
    assert summaries('a.' + extension, code) == [
        ('(anonymous)', 2, 2, 2), ('outer', 1, 4, outer_ccn), ('after', 5, 5, 1),
    ]


@pytest.mark.parametrize('extension', ['ts', 'tsx', 'vue'])
@pytest.mark.parametrize('return_type', ['boolean', '"ok"', '`ok`'])
def test_function_type_alias_and_return_keep_the_real_body(extension, return_type):
    code = (
        f'type Callback = (event: Obj) => {return_type};\n'
        f'function filter(value: Json): (event: Obj) => {return_type} {{\n'
        '  if (value) return () => true;\n'
        '  return () => true;\n'
        '}\n'
        'function after(): void {}\n'
    )
    code, offset = language_source(extension, code)
    assert summaries('a.' + extension, code) == [
        ('(anonymous)', 3 + offset, 3 + offset, 1),
        ('(anonymous)', 4 + offset, 4 + offset, 1),
        ('filter', 2 + offset, 5 + offset, 2),
        ('after', 6 + offset, 6 + offset, 1),
    ]


@pytest.mark.parametrize('extension', ['ts', 'tsx', 'vue'])
@pytest.mark.parametrize('body,ccn', [
    ('/[?:]/.test(x) ? true : false', 2),
    ('`value?${x}:end`', 1),
])
def test_signature_lookahead_preserves_lexical_tokens_and_metrics(extension,
                                                                 body, ccn):
    code = (
        'function outer(c, d) {\n'
        f'  const f = c ? (x): unknown => {body} : y => y || d;\n'
        '  return 1;\n'
        '}\n'
        'function after() { return 1; }\n'
    )
    code, offset = language_source(extension, code)
    assert summaries('a.' + extension, code) == [
        ('(anonymous)', 2 + offset, 2 + offset, ccn),
        ('(anonymous)', 2 + offset, 2 + offset, 2),
        ('outer', 1 + offset, 4 + offset, 2),
        ('after', 5 + offset, 5 + offset, 1),
    ]


@pytest.mark.parametrize('extension', ['ts', 'tsx', 'vue'])
@pytest.mark.parametrize('value', ['call()', '(d)'])
def test_nested_runtime_separator_keeps_following_type_annotation(extension,
                                                                  value):
    code = (
        'function outer(a, c, d) {\n'
        f'  const f = a ? c ? {value} : y => y : z => z || a;\n'
        '  const cb: (e: Obj) => number = e => e || a;\n'
        '  return 1;\n'
        '}\n'
        'function after() { return 1; }\n'
    )
    code, offset = language_source(extension, code)
    assert summaries('a.' + extension, code) == [
        ('(anonymous)', 2 + offset, 2 + offset, 1),
        ('(anonymous)', 2 + offset, 2 + offset, 2),
        ('cb', 3 + offset, 3 + offset, 2),
        ('outer', 1 + offset, 5 + offset, 3),
        ('after', 6 + offset, 6 + offset, 1),
    ]


@pytest.mark.parametrize('extension', ['ts', 'tsx', 'vue'])
@pytest.mark.parametrize('parameters', ['(x: number)', '()'])
def test_return_annotation_after_group_trivia(extension, parameters):
    code = (
        'function outer(c, d) {\n'
        f'  const f = c ? {parameters} /* parameters */\n'
        '    : number => 1 : (y) /* parameters */\n'
        '    : number => y || d;\n'
        '  return 1;\n}\nfunction after() { return 1; }\n'
    )
    code, offset = language_source(extension, code)
    assert summaries('a.' + extension, code) == [
        ('(anonymous)', 3 + offset, 3 + offset, 1),
        ('(anonymous)', 4 + offset, 4 + offset, 2),
        ('outer', 1 + offset, 6 + offset, 2),
        ('after', 7 + offset, 7 + offset, 1),
    ]
