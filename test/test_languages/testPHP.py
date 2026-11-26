import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_languages import PHPReader
from .php_helpers import (
    get_php_function_list,
    get_php_function_list_with_nesting_depth,
)


class Test_tokenizing_PHP(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(PHPReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_empty(self):
        self.check_tokens([], '')

    def test_no_code_block(self):
        self.check_tokens(['"<html></html>"'], '<html></html>')

    def test_empty_code_block(self):
        self.check_tokens([], '<?php?>')

    def test_empty_code_block_with_html(self):
        self.check_tokens(['"<html>"', '"</html>"'], '<html><?php?></html>')

    def test_code_block_with_html(self):
        self.check_tokens(['"<html>"', 'a', '=', '1', '"</html>"'], '<html><?phpa=1?></html>')

    def test_empty_simple_code_block(self):
        self.check_tokens([], '<??>')

    def test_c_comments(self):
        self.check_tokens(['/* this is a comment\nwith two lines*/'], "<?php/* this is a comment\nwith two lines*/?>")

    def test_multiple_line_string(self):
        self.check_tokens(['"this is a string\nwith two lines"'], '<?php"this is a string\nwith two lines"?>')

    def test_heredoc_token(self):
        literal = '<<<NOTICE\nThis is a notice.\nNOTICE'
        self.check_tokens(['\n', 'echo', ' ', literal, ';', '\n'],
                          '<?php\necho ' + literal + ';\n')

    def test_dollar_var(self):
        self.check_tokens(['$a'], '<?$a?>')

    def test_code_block_without_closing(self):
        self.check_tokens(['token'], '<?token')


class TestPHPOperatorNestingDepth(unittest.TestCase):
    """Operators that contain '?' but are not ternary do not add nesting depth."""

    def test_question_operators_do_not_increase_nesting_depth(self):
        samples = (
            "<?php function f() { return $x ?? ''; } ?>",
            "<?php function f() { $x ??= 5; } ?>",
            "<?php function f() { return $obj?->name; } ?>",
            "<?php function f() { return $a ?: 'n'; } ?>",
        )
        for code in samples:
            with self.subTest(code=code):
                functions = get_php_function_list_with_nesting_depth(code)
                self.assertEqual(0, functions[0].max_nesting_depth)
                self.assertEqual(1, functions[0].cyclomatic_complexity)

    def test_multiple_null_coalescing_does_not_accumulate_nesting_depth(self):
        functions = get_php_function_list_with_nesting_depth("""<?php
        function f() {
            return http_build_query([
                'a' => $x['a'] ?? '',
                'b' => $x['b'] ?? '',
                'c' => $x['c'] ?? '',
            ]);
        }
        ?>""")
        self.assertEqual(0, functions[0].max_nesting_depth)

    def test_ternary_still_counts_as_nesting_depth(self):
        functions = get_php_function_list_with_nesting_depth(
            "<?php function f() { return $a ? 'y' : 'n'; } ?>")
        self.assertEqual(1, functions[0].max_nesting_depth)


class TestPHPStrings(unittest.TestCase):

    def test_quoted_multiline_strings(self):
        for label in ("'NOTICE'", '"NOTICE"'):
            with self.subTest(label=label):
                source = '''<?php
function adminNotice() {
    return <<<LABEL
        Show this warning if maintenance is active.
        NOTICE;
}
function isAdmin($user) {
    return $user->isAdmin();
}
'''.replace('LABEL', label)
                functions = get_php_function_list(source)
                self.assertEqual([('adminNotice', 2, 6, 1), ('isAdmin', 7, 9, 1)],
                                 [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                                  for f in functions])

    def test_nested_heredoc_delimiter(self):
        for expression in ('<<<NOTICE\nMaintenance starts at midnight.\nNOTICE',
                           '"Maintenance starts at midnight.\nNOTICE\nCheck the status page."'):
            with self.subTest(expression=expression):
                source = '''<?php
function renderNotice($view) {
    return <<<"NOTICE"
{$view->render(EXPRESSION)}
Check the status page if maintenance is active.
NOTICE;
}

function isMaintenanceActive($status) {
    return $status === 'maintenance';
}
'''.replace('EXPRESSION', expression)
                functions = get_php_function_list(source)
                self.assertEqual([('renderNotice', 2, 9, 1), ('isMaintenanceActive', 11, 13, 1)],
                                 [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                                  for f in functions])

    def test_heredoc_expressions(self):
        for argument in ('"{$labels["https://example.org/status"]}"', '#[Template] fn() => "active"'):
            with self.subTest(argument=argument):
                source = '''<?php
function renderNotice($view, $labels) {
    return <<<NOTICE
{$view->render(ARGUMENT)}
Check the status page if maintenance is active.
NOTICE;
}

function renderFallback() {
    return <<<NOTICE
Please contact support if the page is unavailable.
NOTICE;
}
'''.replace('ARGUMENT', argument)
                functions = get_php_function_list(source)
                self.assertEqual([('renderNotice', 2, 7, 1), ('renderFallback', 9, 13, 1)],
                                 [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                                  for f in functions])

    def test_label_in_heredoc_text(self):
        functions = get_php_function_list('''<?php
function updateNotice($version) {
    return <<<NOTICE
        System NOTICE: if version $version is unsupported, contact support.
        NOTICE;
}
''')
        self.assertEqual([('updateNotice', 6, 1)],
                         [(f.name, f.end_line, f.cyclomatic_complexity) for f in functions])

    def test_nowdoc_opener_in_string(self):
        for literal in (
            """`sed 's/REPORT/Preview/' <<<'REPORT'
`""",
            '''"Example: {$view->render("Use the <<<'REPORT'
marker to start a nowdoc.")}"''',
        ):
            with self.subTest(literal=literal):
                source = '''<?php
function previewReport($view) {
    return LITERAL;
}

function formatReport() {
    return <<<REPORT
Show the report if it is ready.
REPORT;
}
'''.replace('LITERAL', literal)
                self.assertIn(literal, list(PHPReader.generate_tokens(source)))
                functions = get_php_function_list(source)
                self.assertEqual([('previewReport', 2, 5, 1), ('formatReport', 7, 11, 1)],
                                 [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                                  for f in functions])


class Test_parser_for_PHP(unittest.TestCase):

    def test_closing_tag_in_string_preserves_later_methods(self):
        for literal in ('"?>"', "'?>'", r'"escaped \" ?>"', "\"first\n?>last\""):
            with self.subTest(literal=literal):
                source = '''<?php
class Example {
    function first() {
        return 1;
    }

    function second() {
        $text = LITERAL;
        return $text;
    }

    function third() {
        return 3;
    }

    function fourth() {
        return 4;
    }
}
'''.replace('LITERAL', literal)
                functions = get_php_function_list(source)
                offset = literal.count('\n')
                self.assertEqual([
                    ('Example::first', 3, 5, 1),
                    ('Example::second', 7, 10 + offset, 1),
                    ('Example::third', 12 + offset, 14 + offset, 1),
                    ('Example::fourth', 16 + offset, 18 + offset, 1),
                ], [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                    for f in functions])

    def test_inline_attributes_keep_function_signatures(self):
        functions = get_php_function_list("""<?php
class TokenStore {
    public function issue(#[SensitiveParameter] string $token): string {
        return hash('sha256', $token);
    }
}

#[Deprecated] function oldLimit(): int {
    return 10;
}
""")
        self.assertEqual([('TokenStore::issue', 3, 5), ('oldLimit', 8, 10)],
                         [(f.name, f.start_line, f.end_line) for f in functions])

    def test_parameter_attribute_closure_keeps_method_metrics(self):
        analyzer = FileAnalyzer(get_extensions(['cognitive', 'nd', 'ns']))
        info = analyzer.analyze_source_code('store.php', """<?php
class Store {
    public function set(
        #[Validate(static function (string $candidate): bool {
            if ($candidate === '') { return false; }
            return true;
        })] string $name
    ): void {}
}
""")
        method, = info.function_list
        self.assertEqual(('Store::set', 1, 0, 0, 0, ['$name']),
                         (method.name, method.cyclomatic_complexity, method.cognitive_complexity,
                          method.max_nesting_depth, method.max_nested_structures, method.parameters))
        self.assertNotIn('$candidate', method.long_name)
        self.assertEqual(6, method.nloc)

    def test_parameter_attribute_forgive_does_not_hide_method(self):
        functions = get_php_function_list("""<?php
class Store {
    public function set(
        #[Validate(static function (string $candidate): bool {
            // #lizard forgive
            return $candidate !== '';
        })] string $name
    ): void {}
}
""")
        self.assertEqual(['Store::set'], [f.name for f in functions])

    def test_attribute_markers_in_nowdoc_stay_text(self):
        functions = get_php_function_list("""<?php
function sample(): string {
    return <<<'EXAMPLE'
    #[SensitiveParameter] if this contains sensitive data.
    EXAMPLE;
}
""")
        self.assertEqual([('sample', 2, 6, 1)], [
            (f.name, f.start_line, f.end_line, f.cyclomatic_complexity) for f in functions
        ])

    def test_real_closing_tag_still_separates_php_from_html(self):
        for closing in ('?>', '// ?>', '# ?>', '/* ?> */ ?>', 'echo $x < 1 ?>'):
            with self.subTest(closing=closing):
                source = '''<p title="unclosed quote>
<?php function first() { return "?>"; } CLOSING
function ignored() {}
<?php function second() { if ($x) { return 2; } } ?>
'''.replace('CLOSING', closing)
                functions = get_php_function_list(source)
                self.assertEqual([('first', 2, 2, 1), ('second', 4, 4, 2)],
                                 [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                                  for f in functions])

    def test_simple_function(self):
        functions = get_php_function_list("<?php function foo(){} ?>")
        self.assertEqual("foo", functions[0].name)

    def test_simple_function_complexity(self):
        functions = get_php_function_list("<?php function foo(){m;if(a);} ?>")
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_simple_function_complexity_elseif(self):
        functions = get_php_function_list("<?php function foo(){m;if(a);elseif(b);} ?>")
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_value_and_reference_parameters_have_separate_slots(self):
        functions = get_php_function_list("""<?php
function applyUpdates(array $source, array &$entries, array $options = [1, 2], string &...$keys): void {
    foreach ($keys as $key) {
        $entries[$key] = $source[$key];
    }
}
""")
        self.assertEqual((4, ['$source', '$entries', '$options', '$keys']),
                         (functions[0].parameter_count, functions[0].parameters))

    def test_closure_parameters_exclude_nested_parameters_and_captures(self):
        functions = get_php_function_list("""<?php
$processed = 0;
$update = function (
    #[MapWith(static function ($item) { return $item; })] array &$entries,
) use (&$processed): void {
    $processed += count($entries);
};
""")
        self.assertEqual((1, ['$entries']), (functions[0].parameter_count, functions[0].parameters))

    def test_function_assigning_to_a_name(self):
        functions = get_php_function_list("<?php $a = function ($a, $b){} ?>")
        self.assertEqual('$a', functions[0].name)

    def test_not_a_function_assigning_to_a_name(self):
        functions = get_php_function_list("<?php abc=3; function (a, b){} ?>")
        self.assertEqual('abc', functions[0].name)

    def test_function_without_name_assign_to_field(self):
        functions = get_php_function_list("<?php a.b.c = function (a, b){} ?>")
        self.assertEqual('c', functions[0].name)

    def test_class(self):
        functions = get_php_function_list("<?php class C{function x(){}} ?>")
        self.assertEqual('C::x', functions[0].name)

    def test_class_mixed(self):
        functions = get_php_function_list("<?php function a(){}; class C{function b(){}} function c(){} ?>")
        self.assertEqual(3, len(functions))

    def test_interface(self):
        functions = get_php_function_list("<?php function a(); ?>")
        self.assertEqual(1, len(functions))
        self.assertEqual('a', functions[0].name)

    def test_interface2(self):
        functions = get_php_function_list("<?php function a(); class C{}?>")
        self.assertEqual(1, len(functions))

    def test_foreach_is_not_a_function(self):
        functions = get_php_function_list("<?php function test() { foreach($items as $item) { echo $item; } } ?>")
        self.assertEqual(1, len(functions))
        self.assertEqual("test", functions[0].name)
        function_names = [f.name for f in functions]
        self.assertNotIn("foreach", function_names)

    def test_modern_php_methods_with_modifiers(self):
        php_code = '''<?php
        class TestClass {
            public function publicMethod(): string {
                return "test";
            }

            private function privateMethod(): void {
                echo "test";
            }

            protected static function staticMethod(int $param): bool {
                return true;
            }
        }
        ?>'''

        functions = get_php_function_list(php_code)
        self.assertEqual(3, len(functions))
        function_names = sorted([f.name for f in functions])
        self.assertIn("TestClass::publicMethod", function_names)
        self.assertIn("TestClass::privateMethod", function_names)
        self.assertIn("TestClass::staticMethod", function_names)

    def test_use_function_statement_does_not_override_function_name(self):
        php_code = '''<?php
use function bar;
class A {
    function foo() {
        return 1;
    }
}
?>'''
        functions = get_php_function_list(php_code)
        self.assertEqual(1, len(functions))
        self.assertEqual('A::foo', functions[0].name)

    def test_multiple_use_function_statements(self):
        php_code = '''<?php
use function bar;
use function baz;

function realFunction() {
    return 1;
}

class MyClass {
    function myMethod() {
        return 2;
    }
}
?>'''
        functions = get_php_function_list(php_code)
        self.assertEqual(2, len(functions))
        function_names = sorted([f.name for f in functions])
        self.assertIn('realFunction', function_names)
        self.assertIn('MyClass::myMethod', function_names)
        self.assertNotIn('bar', function_names)
        self.assertNotIn('baz', function_names)
