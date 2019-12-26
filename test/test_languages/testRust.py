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
