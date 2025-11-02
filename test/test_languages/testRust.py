import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions


def get_rust_fileinfo(source_code):
    return analyze_file.analyze_source_code("a.rs", source_code)


def get_rust_function_list(source_code):
    return get_rust_fileinfo(source_code).function_list


class TestRust(unittest.TestCase):

    def test_main(self):
        result = get_rust_function_list('''
        fn main() {
            println!("Hello, world!");
        }
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual('main', result[0].name)

    def test_return(self):
        result = get_rust_function_list('''
        fn plus_one(x: i32) -> i32 {
            x + 1;
        }
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual('plus_one', result[0].name)

    def test_if(self):
        result = get_rust_function_list('''
        fn main() {
            match a() {}
        }
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_generic(self):
        result = get_rust_function_list('''
        fn largest<T>(list: &[T]) -> T {
            let mut largest = list[0];

            for &item in list.iter() {
                if item > largest {
                    largest = item;
                }
            }

            largest
        }

        fn main() {
            match a() {}
        }
        ''')
        self.assertEqual(2, len(result))
        self.assertEqual('largest', result[0].name)
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_generic_with_where(self):
        result = get_rust_function_list('''
        fn some_function<T, U>(t: T, u: U) -> i32
            where T: Display + Clone,
                  U: Clone + Debug {
                  }
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_nested_functions(self):
        result = get_rust_function_list('''
        fn main() {
            let x = 4;

            fn equal_to_x(z: i32) -> bool { z == x }

            let y = 4;

            assert!(equal_to_x(y));
        }
        ''')
        self.assertEqual(2, len(result))

    def test_lifetime(self):
        result = get_rust_function_list('''
        pub fn func<'a>(a: &'a i64)
        {
            _ = a
        }
        ''')

        self.assertEqual(1, len(result))


class Test_Rust_Cognitive_Complexity(unittest.TestCase):
    """Cognitive Complexity tests for Rust"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        fn simple(x: i32) -> i32 {
            return x + 1;
        }
        '''
        functions = get_rust_function_list(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        fn check(x: i32) -> &'static str {
            if x > 0 {  // +1
                return "positive";
            }
            return "non-positive";
        }
        '''
        functions = get_rust_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        fn nested() {
            for i in 1..10 {           // +1
                for j in 1..10 {       // +2 (nesting=1)
                    if i == j {        // +3 (nesting=2)
                        println!("{}", i);
                    }
                }
            }
        }
        // Total CogC = 6
        '''
        functions = get_rust_function_list(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        fn check(a: bool, b: bool, c: bool, d: bool, e: bool) -> i32 {
            if a && b && c {  // +1 for if, +1 for && sequence
                return 1;
            }
            if d || e {       // +1 for if, +1 for || sequence
                return 2;
            }
            return 0;
        }
        // Total CogC = 4
        '''
        functions = get_rust_function_list(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_match_statement_counts_as_one(self):
        """Match (Rust's switch) should count as +1 total"""
        code = '''
        fn classify(x: i32) -> &'static str {
            match x {  // +1
                1 => "one",
                2 => "two",
                3 => "three",
                _ => "other",
            }
        }
        // Total CogC = 1
        '''
        functions = get_rust_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        fn countdown(mut n: i32) -> i32 {
            while n > 0 {  // +1
                n = n - 1;
            }
            return n;
        }
        // Total CogC = 1
        '''
        functions = get_rust_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
