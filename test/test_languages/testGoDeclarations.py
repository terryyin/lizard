import unittest
from .testGo import get_go_function_list


class TestGoDeclarations(unittest.TestCase):

    def test_package_level_function_literal_is_named_by_var(self):
        result = get_go_function_list('''
            var handler = func(a int) { if a > 0 { } }
            func after() { }
                ''')
        self.assertEqual(["handler", "after"], [f.name for f in result])
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(1, result[0].parameter_count)

    def test_package_level_typed_function_var(self):
        result = get_go_function_list('''
            var handler func(int) error = func(a int) error { return nil }
            func after() { }
                ''')
        self.assertEqual(["handler", "after"], [f.name for f in result])

    def test_function_type_var_without_initializer(self):
        result = get_go_function_list('''
            var handler func(int) error
            func after() { }
                ''')
        self.assertEqual(["after"], [f.name for f in result])

    def test_function_literal_in_package_level_composite(self):
        result = get_go_function_list('''
            var table = []Handler{ func() { }, }
            func after() { }
                ''')
        self.assertEqual(["", "after"], [f.name for f in result])

    def test_bare_func_literal_does_not_swallow_following_functions(self):
        result = get_go_function_list('''
            var x = func
            func after() { }
                ''')
        self.assertEqual(["after"], [f.name for f in result])

    def test_type_switch_does_not_end_function_early(self):
        result = get_go_function_list('''
            func f(x interface{}) {
                switch v := x.(type) {
                case int:
                    if v > 0 { }
                }
                if true { }
            }
            func g() { }
                ''')
        self.assertEqual(["f", "g"], [f.name for f in result])
        self.assertEqual(4, result[0].cyclomatic_complexity)

    def test_func_type_in_composite_literal_type(self):
        result = get_go_function_list('''
            var table = map[string]func(){ "a": func() { if true { } }, }
            var list = []func() error{ func() error { return nil } }
            func after() {
                hs := make([]func(), 0)
                var c chan func()
                if true { }
            }
                ''')
        self.assertEqual(["", "", "after"], [f.name for f in result])
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(2, result[2].cyclomatic_complexity)
