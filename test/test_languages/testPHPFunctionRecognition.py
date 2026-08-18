import unittest
from .php_helpers import get_php_function_list


class TestPHPFunctionRecognition(unittest.TestCase):
    """What counts as a PHP function: match, arrow fn, closures, nameless types."""

    def test_arrow_fn_skipped(self):
        funcs = get_php_function_list(
            "<?php $double = fn($x) => $x * 2; ?>")
        self.assertEqual(0, len(funcs))

    def test_match_expression_in_function(self):
        code = (
            "<?php\n"
            "function classify($x) {\n"
            "    return match($x) {\n"
            "        1, 2 => 'small',\n"
            "        3, 4 => 'medium',\n"
            "        default => 'large',\n"
            "    };\n"
            "}\n"
            "?>"
        )
        funcs = get_php_function_list(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("classify", funcs[0].name)

    def test_match_with_nested_parens(self):
        code = (
            "<?php\n"
            "function f($x) {\n"
            "    return match(($x)) { 1 => 'a', default => 'b' };\n"
            "}\n"
            "?>"
        )
        funcs = get_php_function_list(code)
        self.assertEqual(1, len(funcs))

    def test_top_level_if_condition(self):
        code = "<?php if ($x > 0) { echo 'pos'; } ?>"
        funcs = get_php_function_list(code)
        self.assertEqual(0, len(funcs))

    def test_top_level_foreach_with_nested_parens(self):
        code = "<?php foreach (((array)$x) as $i) { echo $i; } ?>"
        funcs = get_php_function_list(code)
        self.assertEqual(0, len(funcs))

    def test_trait_with_brace_only_first(self):
        code = (
            "<?php\n"
            "trait { function noop() {} }\n"
            "?>"
        )
        funcs = get_php_function_list(code)
        self.assertEqual(["noop"], [f.name for f in funcs])

    def test_class_with_brace_only_first(self):
        code = (
            "<?php\n"
            "class { function noop() {} }\n"
            "?>"
        )
        funcs = get_php_function_list(code)
        self.assertEqual(["noop"], [f.name for f in funcs])

    def test_function_args_nested_parens(self):
        code = (
            "<?php\n"
            "function f($x = (1 + 2)) { return $x; }\n"
            "?>"
        )
        funcs = get_php_function_list(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("f", funcs[0].name)

    def test_anonymous_function_in_class(self):
        code = (
            "<?php\n"
            "class C {\n"
            "    public $cb;\n"
            "    public function __construct() {\n"
            "        $this->cb = function() { return 1; };\n"
            "    }\n"
            "}\n"
            "?>"
        )
        funcs = get_php_function_list(code)
        names = [f.name for f in funcs]
        self.assertTrue(any("__construct" in n for n in names))

    def test_anonymous_function_assigned_top_level(self):
        code = (
            "<?php\n"
            "$f = function($x) { return $x + 1; };\n"
            "?>"
        )
        funcs = get_php_function_list(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("$f", funcs[0].name)

    def test_top_level_match_expression(self):
        code = (
            "<?php\n"
            "$r = match(($x)) {\n"
            "    1, 2 => 'small',\n"
            "    3, 4 => 'medium',\n"
            "    default => 'large',\n"
            "};\n"
            "?>"
        )
        funcs = get_php_function_list(code)
        self.assertEqual(0, len(funcs))

    def test_bare_top_level_anonymous_function(self):
        code = (
            "<?php\n"
            "function() { return 1; };\n"
            "?>"
        )
        funcs = get_php_function_list(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("(anonymous)", funcs[0].name)
