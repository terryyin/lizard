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

    def test_trait_declarations_leave_default_method(self):
        result = get_rust_function_list('''
        use std::path::Path;

        pub trait Installable {
            fn install_path(&self) -> &Path;
            fn write_to<W>(&self, writer: W) -> std::io::Result<()>
            where W: std::io::Write;

            fn to_resolution(&self, cached: bool) -> u32 {
                if cached { return 1; }
                0
            }
        }
        ''')
        self.assertEqual(['to_resolution'], [function.name for function in result])
        self.assertEqual((9, 12, 2), (result[0].start_line, result[0].end_line,
                                      result[0].cyclomatic_complexity))

    def test_extern_declaration_leaves_nested_wrapper(self):
        result = get_rust_function_list('''
        pub fn process_id() -> u32 {
            unsafe extern "C" {
                fn getpid() -> i32;
            }
            fn is_valid(pid: i32) -> bool {
                pid > 0
            }
            let pid = unsafe { getpid() };
            if is_valid(pid) { pid as u32 } else { 0 }
        }
        ''')
        self.assertEqual(['is_valid', 'process_id'], [function.name for function in result])
        self.assertEqual(2, result[1].cyclomatic_complexity)
        self.assertEqual(8, result[1].nloc)

    def test_default_method_with_array_return_type_has_a_body(self):
        result = get_rust_function_list('''
        pub trait Header {
            fn checksum(&self) -> [u8; 4];
            fn magic(&self) -> [u8; 4] {
                *b"RIFF"
            }
        }
        ''')
        self.assertEqual(['magic'], [function.name for function in result])
        self.assertEqual((4, 6), (result[0].start_line, result[0].end_line))

    def test_return_type_macro_can_contain_semicolon(self):
        result = get_rust_function_list('''
        macro_rules! packet_type {
            ($item:ty; $size:expr) => { [$item; $size] };
        }

        pub fn header() -> packet_type!(u8; 4) {
            *b"RIFF"
        }
        ''')
        self.assertEqual(['header'], [function.name for function in result])
        self.assertEqual((6, 8), (result[0].start_line, result[0].end_line))

    def test_type_macro_with_raw_byte_string_keeps_function_body(self):
        result = get_rust_function_list('''
        macro_rules! bytes_for {
            ($literal:literal) => { [u8; $literal.len()] };
        }

        pub fn header() -> bytes_for!(br#"RIFF"#)
        where
            (): Sized,
        {
            *b"RIFF"
        }

        pub fn chunk_count() -> usize {
            1
        }
        ''')
        self.assertEqual(['header', 'chunk_count'], [function.name for function in result])
        self.assertEqual((6, 11), (result[0].start_line, result[0].end_line))

    def test_type_macro_with_hidden_opening_bracket_keeps_function(self):
        result = get_rust_function_list('''
        macro_rules! packet_type {
            ($magic:literal, [$($flag:ident),* $(,)?]; $bytes:ty) => { $bytes };
        }

        pub fn header() -> packet_type!(
            br#"RIFF"#, [
                little_endian,
            ]; [u8; 4]
        ) {
            *b"RIFF"
        }

        pub fn chunk_count() -> usize {
            1
        }
        ''')
        self.assertEqual(['header', 'chunk_count'], [function.name for function in result])
        self.assertEqual((6, 12), (result[0].start_line, result[0].end_line))

    def test_forgive_in_foreign_block_stays_with_the_declaration(self):
        result = get_rust_function_list('''
        unsafe extern "C" {
            // #lizard forgives
            fn getpid() -> i32;
        }

        pub struct Process {}

        pub fn pid() -> i32 {
            unsafe { getpid() }
        }

        pub fn parent_pid() -> i32 {
            // #lizard forgives
            unsafe extern "C" {
                fn getppid() -> i32;
            }
            unsafe { getppid() }
        }

        pub fn pid_from_local_binding() -> i32 {
            unsafe extern "C" {
                // #lizard forgives
                fn getpid() -> i32;
            }
            unsafe { getpid() }
        }
        ''')
        self.assertEqual(['pid', 'pid_from_local_binding'], [function.name for function in result])

    def test_type_macro_with_hashless_raw_path_keeps_function(self):
        result = get_rust_function_list(r'''
        macro_rules! path_buf_type {
            ($root:literal, [$($dir:literal),+]; $cap:expr) => { [u8; $cap] };
        }

        pub fn default_path() -> path_buf_type!(r"C:\", ["Program Files"]; 32) {
            [0; 32]
        }

        pub fn default_size() -> usize {
            32
        }
        ''')
        self.assertEqual(['default_path', 'default_size'], [function.name for function in result])
        self.assertEqual((6, 8), (result[0].start_line, result[0].end_line))

    def test_callback_fields_and_foreign_declaration_have_no_body(self):
        result = get_rust_function_list('''
        pub struct Signal {
            pub first: Option<extern "C" fn(i32)>,
            pub second: Option<extern "C" fn(i32)>,
        }
        unsafe extern "C" {
            pub fn signal();
        }
        ''')
        self.assertEqual([], result)

    def test_const_arguments_do_not_start_function_bodies(self):
        result = get_rust_function_list('''pub struct Tag<const N: u8>;

        pub trait Header {
            fn tag() -> Tag<{ b'A' + 1 }>;
            fn size() -> usize { 4 }
        }

        fn header(cached: bool) -> Option<Tag<{ b'A' + 1 }>> {
            if cached { Some(Tag) } else { None }
        }
        ''')
        self.assertEqual(
            [('size', 5, 5, 1), ('header', 8, 10, 2)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_braced_macro_in_return_type_does_not_start_function_body(self):
        result = get_rust_function_list('''macro_rules! header_type {
            ($size:expr) => { [u8; $size] };
        }

        fn magic() -> header_type!{4} {
            *b"RIFF"
        }

        fn wait() -> ! {
            loop { std::thread::park(); }
        }
        ''')
        self.assertEqual(
            [('magic', 5, 7), ('wait', 9, 11)],
            [(f.name, f.start_line, f.end_line) for f in result])

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
