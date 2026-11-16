import unittest
from lizard import analyze_file


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

    def test_empty_match(self):
        result = get_rust_function_list('''
        fn main() {
            match a() {}
        }
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(1, result[0].cyclomatic_complexity)

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

    def test_case_as_identifier(self):
        """
        Test that 'case' used as an identifier doesn't add to CCN.
        Rust doesn't have 'case' keyword (uses match expressions with arms).
        """
        code = '''
        fn handle_case_variable(case: i32) -> i32 {
            let case_value = case;
            match case_value {
                1 => println!("one"),
                2 => println!("two"),
                _ => println!("other"),
            }
            case
        }
        '''
        result = get_rust_function_list(code)
        self.assertEqual(1, len(result))

        # 3 match arms → CCN 3. 'case' as an identifier must not add more.
        self.assertEqual(3, result[0].cyclomatic_complexity,
                        "'case' as identifier doesn't add to CCN")

    def test_match_or_patterns_do_not_add_ccn(self):
        code = '''
        fn categorize(value: i32) -> &'static str {
            match value {
                1 | 2 | 3 => "small",
                4 | 5 => "medium",
                6..=10 => "large",
                _ => "other",
            }
        }
        '''
        result = get_rust_function_list(code)
        self.assertEqual(1, len(result))

        # 4 arms → CCN 4. `|` in patterns is not a logical operator.
        self.assertEqual(4, result[0].cyclomatic_complexity)

    def test_match_and_equivalent_if_else_chain_have_same_ccn(self):
        result = get_rust_function_list('''
        fn classify(n: i32) -> &'static str {
            match n {
                0 => "zero",
                1 => "one",
                2 => "two",
                3 => "three",
                4 => "four",
                5 => "five",
                _ => "many",
            }
        }

        fn classify_if(n: i32) -> &'static str {
            if n == 0 {
                "zero"
            } else if n == 1 {
                "one"
            } else if n == 2 {
                "two"
            } else if n == 3 {
                "three"
            } else if n == 4 {
                "four"
            } else if n == 5 {
                "five"
            } else {
                "many"
            }
        }
        ''')
        self.assertEqual(7, result[0].cyclomatic_complexity)
        self.assertEqual(7, result[1].cyclomatic_complexity)

    def test_match_ccn_equals_arm_count(self):
        for arm_count in (2, 5, 10):
            arms = ",\n".join(
                '%s => "%s"' % (i, i) for i in range(arm_count - 1)
            )
            arms += ',\n_ => "other"'
            code = '''
            fn f(n: i32) -> &'static str {
                match n {
                    %s
                }
            }
            ''' % arms
            result = get_rust_function_list(code)
            self.assertEqual(
                arm_count, result[0].cyclomatic_complexity,
                "%s match arms" % arm_count)

    def test_nested_match_adds_inner_arms(self):
        result = get_rust_function_list('''
        fn classify(x: i32, y: i32) -> i32 {
            match x {
                1 => match y {
                    10 => 1,
                    _ => 2,
                },
                _ => 3,
            }
        }
        ''')
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_match_with_parenthesized_struct_subject(self):
        result = get_rust_function_list('''
        fn classify() -> i32 {
            match (Foo { a: 1 }) {
                0 => 0,
                1 => 1,
                _ => 2,
            }
        }
        ''')
        self.assertEqual(3, result[0].cyclomatic_complexity)
