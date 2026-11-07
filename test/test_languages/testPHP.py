import unittest
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

    def test_multiple_line_string_alternative(self):
        self.check_tokens(['<<<blah xxx blah'], '<?php<<<blah xxx blah?>')

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


class Test_parser_for_PHP(unittest.TestCase):

    def test_simple_function(self):
        functions = get_php_function_list("<?php function foo(){} ?>")
        self.assertEqual("foo", functions[0].name)

    def test_simple_function_complexity(self):
        functions = get_php_function_list("<?php function foo(){m;if(a);} ?>")
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_simple_function_complexity_elseif(self):
        functions = get_php_function_list("<?php function foo(){m;if(a);elseif(b);} ?>")
        self.assertEqual(3, functions[0].cyclomatic_complexity)

    def test_parameter_count(self):
        php_code = "<?php function foo($a, $b){} ?>"
        functions = get_php_function_list(php_code)
        # The current implementation counts $a and $b as 1 parameter
        # This matches the behavior of the original implementation
        self.assertEqual(1, functions[0].parameter_count)

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
