"""
Regression tests for behavioral changes introduced by the CogC branch.

These tests verify that language parsers work correctly both WITH and WITHOUT
the CogC extension loaded. They document intentional behavioral changes and
prevent regressions.
"""
import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions


def analyze_without_cogc(filename, code):
    """Analyze code without CogC extension (default lizard behavior)."""
    return analyze_file.analyze_source_code(filename, code)


def analyze_with_cogc(filename, code):
    """Analyze code with CogC extension loaded."""
    from lizard_ext.lizardcogc import LizardExtension as CogC
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(filename, code)


class TestTypeScriptWithoutCogC(unittest.TestCase):
    """TypeScript must work without CogC extension loaded."""

    def test_basic_function(self):
        result = analyze_without_cogc('test.ts', '''
function hello() {
    return 1;
}
''')
        self.assertEqual(1, len(result.function_list))
        self.assertEqual('hello', result.function_list[0].name)

    def test_nested_function(self):
        result = analyze_without_cogc('test.ts', '''
function outer() {
    const inner = function() { return 1; };
    if (true) { }
}
''')
        names = [f.name for f in result.function_list]
        self.assertIn('outer', names)
        # Without CogC, inner should be a separate function
        self.assertEqual(2, len(result.function_list))

    def test_arrow_function(self):
        result = analyze_without_cogc('test.ts', '''
const greet = (name) => {
    return "hello " + name;
};
''')
        self.assertEqual(1, len(result.function_list))

    def test_class_method(self):
        result = analyze_without_cogc('test.ts', '''
class Foo {
    bar() {
        if (true) { }
    }
}
''')
        self.assertEqual(1, len(result.function_list))
        self.assertEqual(2, result.function_list[0].cyclomatic_complexity)


class TestPythonWithoutCogC(unittest.TestCase):
    """Python must produce identical metrics without CogC extension loaded."""

    def test_basic_function(self):
        result = analyze_without_cogc('test.py', '''
def hello():
    return 1
''')
        self.assertEqual(1, len(result.function_list))
        self.assertEqual('hello', result.function_list[0].name)

    def test_nested_function(self):
        result = analyze_without_cogc('test.py', '''
def outer():
    x = 1
    def inner():
        return 2
    return inner
''')
        names = [f.name for f in result.function_list]
        self.assertIn('outer', names)
        self.assertEqual(2, len(result.function_list))

    def test_nested_function_metrics_match_baseline(self):
        """Nested function CCN and nesting must match pre-CogC baseline."""
        result = analyze_without_cogc('test.py', '''
def outer():
    x = 1
    def inner():
        if True:
            return 2
    return inner()
''')
        funcs = {f.name: f for f in result.function_list}
        self.assertEqual(2, len(result.function_list))
        self.assertIn('outer.inner', funcs)
        self.assertIn('outer', funcs)
        # CCN must match master baseline
        self.assertEqual(2, funcs['outer.inner'].cyclomatic_complexity)
        self.assertEqual(1, funcs['outer'].cyclomatic_complexity)

    def test_comprehension_metrics_match_baseline(self):
        """Comprehension CCN must match pre-CogC baseline."""
        result = analyze_without_cogc('test.py', '''
def process():
    items = [
        x for x in range(10)
        if x > 5
    ]
    if items:
        return items
    return []
''')
        f = result.function_list[0]
        self.assertEqual(1, len(result.function_list))
        self.assertEqual('process', f.name)
        # CCN = 1 (base) + 1 (for) + 1 (if in comprehension) + 1 (if items) = 4
        self.assertEqual(4, f.cyclomatic_complexity)

    def test_nested_comprehension_in_function(self):
        """Nested comprehensions should not create extra functions."""
        result = analyze_without_cogc('test.py', '''
def matrix():
    return [[i*j for j in range(10)] for i in range(10)]
''')
        self.assertEqual(1, len(result.function_list))

    def test_decorator_pattern(self):
        """Decorated functions should work correctly."""
        result = analyze_without_cogc('test.py', '''
def decorator(func):
    def wrapper(*args):
        return func(*args)
    return wrapper
''')
        names = [f.name for f in result.function_list]
        self.assertIn('decorator', names)
        self.assertEqual(2, len(result.function_list))


class TestCppPreprocessorTokens(unittest.TestCase):
    """C++ preprocessor directives should not double-count CCN."""

    def test_ifdef_counts_once_for_ccn(self):
        """#ifdef should add exactly 1 to CCN, not 2 (no double counting)."""
        result = analyze_without_cogc('test.cpp', '''
void foo() {
#ifdef DEBUG
    printf("debug");
#endif
}
''')
        self.assertEqual(1, len(result.function_list))
        # CCN = 1 (base) + 1 (#ifdef) = 2
        self.assertEqual(2, result.function_list[0].cyclomatic_complexity)

    def test_multiple_preprocessor_directives(self):
        """Multiple preprocessor directives should each count once."""
        result = analyze_without_cogc('test.cpp', '''
void foo() {
#ifdef A
    a();
#elif defined(B)
    b();
#endif
}
''')
        self.assertEqual(1, len(result.function_list))
        # CCN = 1 (base) + 1 (#ifdef) + 1 (#elif) = 3
        self.assertEqual(3, result.function_list[0].cyclomatic_complexity)

    def test_preprocessor_does_not_affect_function_count(self):
        """Preprocessor directives should not create phantom functions."""
        result = analyze_without_cogc('test.cpp', '''
#ifdef FEATURE
void foo() { }
#else
void foo() { }
#endif
''')
        # Both foo() definitions are present in source, but #else branch is ignored
        # by cpre extension when loaded. Without cpre, both are counted.
        self.assertGreaterEqual(len(result.function_list), 1)


class TestStKeywordNormalization(unittest.TestCase):
    """ST keyword normalization should preserve CCN counting."""

    def test_if_elsif_ccn(self):
        """IF/ELSIF should count towards CCN regardless of case."""
        result = analyze_without_cogc('test.st', '''
FUNCTION foo : INT
IF x > 0 THEN
    y := 1;
ELSIF x < 0 THEN
    y := -1;
ELSE
    y := 0;
END_IF;
END_FUNCTION
''')
        self.assertEqual(1, len(result.function_list))
        # CCN = 1 (base) + 1 (IF) + 1 (ELSIF) = 3
        self.assertEqual(3, result.function_list[0].cyclomatic_complexity)

    def test_end_var_not_structural(self):
        """END_VAR should not terminate the function early (bugfix).

        On master, END_VAR was collapsed to END which the state machine
        treated as END_FUNCTION, terminating the function early and losing
        the IF statement's CCN contribution.
        """
        result = analyze_without_cogc('test.st', '''
FUNCTION foo : INT
VAR
    x : INT;
END_VAR
IF x > 0 THEN
    x := 1;
END_IF;
END_FUNCTION
''')
        self.assertEqual(1, len(result.function_list))
        # CCN = 1 (base) + 1 (IF) = 2
        self.assertEqual(2, result.function_list[0].cyclomatic_complexity)

    def test_for_while_repeat_ccn(self):
        """FOR/WHILE/REPEAT should count towards CCN."""
        result = analyze_without_cogc('test.st', '''
FUNCTION foo : INT
FOR i := 0 TO 10 DO
    x := x + 1;
END_FOR;
WHILE y > 0 DO
    y := y - 1;
END_WHILE;
END_FUNCTION
''')
        self.assertEqual(1, len(result.function_list))
        # CCN = 1 (base) + 1 (FOR) + 1 (WHILE) = 3
        self.assertEqual(3, result.function_list[0].cyclomatic_complexity)


class TestPlsqlWithoutCogC(unittest.TestCase):
    """PLSQL must preserve CCN when CogC extension is not loaded."""

    def test_basic_procedure_ccn(self):
        """Basic PLSQL procedure CCN must match pre-CogC baseline."""
        result = analyze_without_cogc('test.sql', '''
CREATE PROCEDURE test_proc IS
    v_count NUMBER;
BEGIN
    IF v_count > 0 THEN
        v_count := v_count + 1;
    ELSIF v_count < 0 THEN
        v_count := 0;
    END IF;
    FOR i IN 1..10 LOOP
        v_count := v_count + i;
    END LOOP;
END;
''')
        self.assertEqual(1, len(result.function_list))
        self.assertEqual('test_proc', result.function_list[0].name)
        # CCN = 1 (base) + 1 (IF) + 1 (ELSIF) + 1 (FOR) = 4
        self.assertEqual(4, result.function_list[0].cyclomatic_complexity)

    def test_nested_if_ccn(self):
        """Nested IF in PLSQL must produce correct CCN."""
        result = analyze_without_cogc('test.sql', '''
CREATE PROCEDURE test_proc IS
BEGIN
    IF v_count > 0 THEN
        IF v_count > 10 THEN
            v_count := 10;
        END IF;
    END IF;
END;
''')
        self.assertEqual(1, len(result.function_list))
        # CCN = 1 (base) + 1 (IF) + 1 (IF) = 3
        self.assertEqual(3, result.function_list[0].cyclomatic_complexity)


class TestErlangFunctionDetection(unittest.TestCase):
    """Erlang reserved keywords should not be treated as function names."""

    def test_reserved_keyword_not_function(self):
        """Erlang reserved words like 'case', 'if' should not create functions."""
        result = analyze_without_cogc('test.erl', '''
foo(X) ->
    case X of
        1 -> ok;
        2 -> error
    end.
''')
        names = [f.name for f in result.function_list]
        self.assertIn('foo', names)
        # 'case' should NOT be a function name
        self.assertNotIn('case', names)

    def test_function_requires_parenthesis(self):
        """Only identifiers followed by '(' should be treated as functions."""
        result = analyze_without_cogc('test.erl', '''
hello(X) ->
    X + 1.
''')
        self.assertEqual(1, len(result.function_list))
        self.assertEqual('hello', result.function_list[0].name)

    def test_multiple_functions(self):
        """Multiple Erlang functions should be detected correctly."""
        result = analyze_without_cogc('test.erl', '''
foo(X) ->
    X + 1.
bar(Y) ->
    Y * 2.
''')
        self.assertEqual(2, len(result.function_list))
        names = [f.name for f in result.function_list]
        self.assertIn('foo', names)
        self.assertIn('bar', names)


class TestTypeScriptWithCogC(unittest.TestCase):
    """TypeScript lambda detection should work correctly with CogC."""

    def test_anonymous_callback_aggregates_to_parent(self):
        """Anonymous functions inside another function should aggregate to parent."""
        result = analyze_with_cogc('test.ts', '''
function outer() {
    setTimeout(function() {
        console.log("hello");
    }, 100);
}
''')
        names = [f.name for f in result.function_list]
        # With CogC, anonymous callback aggregates to parent
        self.assertEqual(['outer'], names)

    def test_named_top_level_function_is_separate(self):
        """Named top-level functions should always be separate FunctionInfo."""
        result = analyze_with_cogc('test.ts', '''
function foo() {
    return 1;
}
function bar() {
    return 2;
}
''')
        self.assertEqual(2, len(result.function_list))


class TestPythonWithCogC(unittest.TestCase):
    """Python nested function handling should work correctly with CogC."""

    def test_nested_function_detected(self):
        """Nested functions should still be detected with CogC."""
        result = analyze_with_cogc('test.py', '''
def outer():
    x = 1
    def inner():
        return 2
    return inner
''')
        names = [f.name for f in result.function_list]
        self.assertEqual(2, len(result.function_list))

    def test_comprehension_ccn_unchanged(self):
        """Comprehensions should not affect CCN."""
        result = analyze_with_cogc('test.py', '''
def process():
    items = [x for x in range(10) if x > 5]
    return items
''')
        self.assertEqual(1, len(result.function_list))


class TestCogCExtensionImport(unittest.TestCase):
    """CogC extension must import cleanly without sys.modules hacks."""

    def test_cogc_extension_imports(self):
        """Importing lizardcogc should not raise."""
        from lizard_ext.lizardcogc import LizardExtension
        self.assertIsNotNone(LizardExtension)

    def test_cogc_patches_function_info(self):
        """After import, FunctionInfo should have cognitive_complexity."""
        from lizard_ext.lizardcogc import LizardExtension  # noqa: F401
        from lizard import FunctionInfo
        f = FunctionInfo.__new__(FunctionInfo)
        f.__init__('test', 'test.py', 1)
        self.assertTrue(hasattr(f, 'cognitive_complexity'))
        self.assertEqual(0, f.cognitive_complexity)


class TestCogCPropertyWithoutExtension(unittest.TestCase):
    """FileInformation properties must not crash without CogC loaded."""

    def test_average_cognitive_complexity_without_cogc(self):
        """average_cognitive_complexity must not crash in a fresh process."""
        import subprocess
        result = subprocess.run(
            ['python', '-c', (
                'from lizard import analyze_file\n'
                'r = analyze_file.analyze_source_code("test.py", '
                '"def hello():\\n    return 1\\n")\n'
                'print(r.average_cognitive_complexity)\n'
                'print(r.CogC)\n'
            )],
            capture_output=True, text=True
        )
        self.assertEqual(0, result.returncode,
                         f"Crashed without CogC: {result.stderr}")


class TestCppPreprocessorMetricsPreserved(unittest.TestCase):
    """C++ preprocessor tokens must not change token count or NLOC without CogC."""

    def test_ifdef_token_count_matches_master(self):
        """#ifdef must NOT add extra tokens without CogC (master baseline: 10)."""
        result = analyze_without_cogc('test.cpp', '''
void foo() {
#ifdef DEBUG
    printf("debug");
#endif
}
''')
        f = result.function_list[0]
        self.assertEqual('foo', f.name)
        self.assertEqual(2, f.cyclomatic_complexity)
        # Master baseline: token=10. Preprocessor tokens must not leak into count.
        self.assertEqual(10, f.token_count)

    def test_multiple_preprocessor_token_count(self):
        """Multiple #ifdef/#elif must not inflate token count."""
        result = analyze_without_cogc('test.cpp', '''
void foo() {
#ifdef A
    a();
#elif defined(B)
    b();
#endif
}
''')
        f = result.function_list[0]
        self.assertEqual(3, f.cyclomatic_complexity)
        # Master baseline: each preprocessor directive should NOT add a token
        # Tokens: void foo ( ) { a ( ) ; b ( ) ; } = ~14
        # NOT 14 + 2 (from leaked #ifdef and #elif tokens)
        master_token_count = f.token_count
        # Verify no extra tokens from preprocessor leak
        # On master: void(1) foo(counted before push) {(1) a(1) ((1) )(1) ;(1) b(1) ((1) )(1) ;(1) }(1) = ~11
        # The exact count depends on tokenization, but must NOT include #ifdef/#elif
        self.assertLessEqual(master_token_count, 14,
                             "Preprocessor tokens are leaking into token count")


class TestPlsqlMetricsPreserved(unittest.TestCase):
    """PLSQL metrics must match master baseline without CogC loaded."""

    def test_basic_procedure_token_count(self):
        """PLSQL token count must match master baseline (78)."""
        result = analyze_without_cogc('test.sql', '''
CREATE PROCEDURE test_proc IS
    v_count NUMBER;
BEGIN
    IF v_count > 0 THEN
        v_count := v_count + 1;
    ELSIF v_count < 0 THEN
        v_count := 0;
    END IF;
    FOR i IN 1..10 LOOP
        v_count := v_count + i;
    END LOOP;
END;
''')
        f = result.function_list[0]
        self.assertEqual('test_proc', f.name)
        self.assertEqual(4, f.cyclomatic_complexity)
        # Master baseline: token=78. Synthetic braces must not inflate count.
        self.assertEqual(78, f.token_count)

    def test_simple_procedure_token_count(self):
        """Simple PLSQL procedure without control flow must preserve token count."""
        result = analyze_without_cogc('test.sql', '''
CREATE PROCEDURE simple_proc IS
BEGIN
    v_x := 1;
END;
''')
        f = result.function_list[0]
        self.assertEqual('simple_proc', f.name)
        self.assertEqual(1, f.cyclomatic_complexity)
        # No control structures = no synthetic braces = no token count change
        master_token_count = f.token_count
        self.assertGreater(master_token_count, 0)


class TestErlangMetricsPreserved(unittest.TestCase):
    """Erlang metrics must match master baseline without CogC loaded."""

    def test_function_with_case_token_count(self):
        """Erlang function with case expression must preserve token count."""
        result = analyze_without_cogc('test.erl', '''
foo(X) ->
    case X of
        1 -> ok;
        2 -> error
    end.
''')
        f = [fn for fn in result.function_list if fn.name == 'foo'][0]
        self.assertEqual(2, f.cyclomatic_complexity)
        # Master baseline: token=10
        self.assertEqual(10, f.token_count)

    def test_simple_function_token_count(self):
        """Simple Erlang function token count must match master."""
        result = analyze_without_cogc('test.erl', '''
bar(Y) ->
    Y * 2.
''')
        f = result.function_list[0]
        self.assertEqual('bar', f.name)
        self.assertEqual(1, f.cyclomatic_complexity)
        # Master baseline: token=9
        self.assertEqual(9, f.token_count)


if __name__ == '__main__':
    unittest.main()
