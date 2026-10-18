import unittest
from lizard import analyze_file


def get_rust_function_list(source_code):
    return analyze_file.analyze_source_code("a.rs", source_code).function_list


class TestRustLetElse(unittest.TestCase):

    def test_let_else_is_a_decision(self):
        result = get_rust_function_list('''
        fn first(v: &[i32]) -> i32 {
            let Some(x) = v.first() else { return 0; };
            *x
        }
        ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_if_let_else_is_one_decision(self):
        result = get_rust_function_list('''
        fn first(v: &[i32]) -> i32 {
            if let Some(x) = v.first() { *x } else { return 0; }
        }
        ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_else_of_initializer_if_is_not_let_else(self):
        result = get_rust_function_list('''
        fn value(flag: bool) -> i32 {
            let x = if flag { 1 } else { 2 };
            x
        }
        ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_let_else_after_if_initializer_adds_its_own_decision(self):
        result = get_rust_function_list('''
        fn first(v: &[i32], flag: bool) -> i32 {
            let Some(x) = if flag { v.first() } else { None } else {
                return 0;
            };
            *x
        }
        ''')
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_if_then_let_else_are_separate_decisions(self):
        result = get_rust_function_list('''
        fn first(v: &[i32], flag: bool) -> i32 {
            if flag { return 1; }
            let Some(x) = v.first() else { return 0; };
            *x
        }
        ''')
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_parenthesized_nested_if_else_binds_to_if(self):
        result = get_rust_function_list('''
        fn value(flag: bool) -> i32 {
            if (if flag { 1 } else { 0 }) > 0 { 1 } else { 0 }
        }
        ''')
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_let_else_struct_pattern(self):
        result = get_rust_function_list('''
        fn field(value: Foo) -> i32 {
            let Foo { x } = value else { return 0; };
            x
        }
        ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_if_else_with_parenthesized_struct_condition(self):
        result = get_rust_function_list('''
        fn value(x: Foo) -> i32 {
            if (Foo { a: 1 }) == x { 1 } else { 0 }
        }
        ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)
