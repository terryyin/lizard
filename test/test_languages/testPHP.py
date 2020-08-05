import unittest
from lizard import  analyze_file, FileAnalyzer, get_extensions
from lizard_languages import PHPReader


def get_php_function_list(source_code):
    return analyze_file.analyze_source_code("a.php", source_code).function_list


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
        functions = get_php_function_list("<?php function foo($a, $b){} ?>")
        self.assertEqual(2, functions[0].parameter_count)

    def test_function_assigning_to_a_name(self):
        functions = get_php_function_list("<?php $a = function ($a, $b){} ?>")
        self.assertEqual('$a', functions[0].name)

    def test_not_a_function_assigning_to_a_name(self):
        functions = get_php_function_list("<?php abc=3; function (a, b){} ?>")
        self.assertEqual('(anonymous)', functions[0].name)

    def test_function_without_name_assign_to_field(self):
        functions = get_php_function_list("<?php a.b.c = function (a, b){} ?>")
        self.assertEqual('a.b.c', functions[0].name)

    def test_class(self):
        functions = get_php_function_list("<?php class C{function x(){}} ?>")
        self.assertEqual('x', functions[0].name)

    def test_class_mixed(self):
        functions = get_php_function_list("<?php function a(){}; class C{function b(){}} function c(){} ?>")
        self.assertEqual(3, len(functions))

    def test_interface(self):
        functions = get_php_function_list("<?php function a(); ?>")
        self.assertEqual(0, len(functions))

    def test_interface2(self):
        functions = get_php_function_list("<?php function a(); class C{}?>")
        self.assertEqual(0, len(functions))
