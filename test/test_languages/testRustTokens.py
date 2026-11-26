"""Rust tokenization keeps function metrics and extension inputs coherent."""

import unittest

from lizard import FileAnalyzer, get_extensions
from .testRust import get_rust_function_list


class TestRustTokens(unittest.TestCase):

    def test_raw_byte_string_keeps_the_return_type_macro_boundary(self):
        result = get_rust_function_list('''macro_rules! bytes {
            ($tag:literal) => { [u8; $tag.len()] };
        }

        pub fn header() -> bytes!(br#"RIFF"#) {
            *b"RIFF"
        }

        pub fn chunk_count() -> usize {
            4
        }
        ''')
        self.assertEqual(
            [('header', 5, 7), ('chunk_count', 9, 11)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_raw_text_string_counts_as_one_token(self):
        result = get_rust_function_list('''fn example_source() -> &'static str {
            r#"fn greet() { println!("hello"); }"#
        }
        ''')
        self.assertEqual(
            [('example_source', 1, 3, 11, 1)],
            [(f.name, f.start_line, f.end_line, f.token_count,
              f.cyclomatic_complexity) for f in result])

    def test_raw_c_prefix_does_not_hide_later_functions(self):
        for source in (
                r'''macro_rules! save { (cr # $text:literal #) => {}; }
                fn first() {
                    save!(cr#"a\"#b"#);
                }
                fn next() { let _ = "ok"; }
                ''',
                r'''macro_rules! save { ($raw:literal $text:literal) => {}; }
                fn first() {
                    save!(cr#"text"#r"a\"b");
                }
                fn next() { let _ = "ok"; }
                '''):
            with self.subTest(source=source):
                result = get_rust_function_list(source)
                self.assertEqual(
                    [('first', 2, 4), ('next', 5, 5)],
                    [(f.name, f.start_line, f.end_line) for f in result])

    def test_macro_interpolation_keeps_the_enclosing_range(self):
        result = get_rust_function_list('''macro_rules! capture {
            ($($tokens:tt)*) => { stringify!($($tokens)*) };
        }
        fn fields() -> &'static str {
            capture! {
                Record { fields: #fields } }
        }
        fn after() {}
        ''')
        self.assertEqual(
            [('fields', 4, 7), ('after', 8, 8)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_repeated_macro_interpolation_keeps_the_enclosing_range(self):
        result = get_rust_function_list('''macro_rules! capture {
            ($($tokens:tt)*) => { stringify!($($tokens)*) };
        }
        fn fields() -> &'static str {
            capture! { #( #fields ),* }
        }
        fn after() {}
        ''')
        self.assertEqual(
            [('fields', 4, 6), ('after', 7, 7)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_executable_macro_keeps_its_condition(self):
        analyzer = FileAnalyzer(get_extensions(['ND', 'cognitive']))
        result = analyzer.analyze_source_code('macro.rs', '''macro_rules! run {
            ($value:expr) => { $value };
        }
        fn choose(flag: bool) -> usize {
            run! { if flag { 1 } else { 0 } }
        }
        ''').function_list
        self.assertEqual(
            [('choose', 4, 6, 2, 1, 2)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
              f.max_nesting_depth, f.cognitive_complexity) for f in result])

    def test_closure_without_parameters_is_one_token(self):
        # `||` of a closure without parameters is one token of the source,
        # and not a logical or.
        empty, named = (get_rust_function_list(
            'fn f(v: Option<i32>) -> i32 {\n    v.unwrap_or_else(%s 0)\n}\n' % bars)[0]
            for bars in ('||', '|_|'))
        self.assertEqual(named.token_count - 2, empty.token_count)
        self.assertEqual(1, empty.cyclomatic_complexity)

    def test_char_literal_does_not_hide_the_functions_after_it(self):
        result = get_rust_function_list('''
        fn is_call(code: char) -> bool {
            code == 'C'
        }

        fn either(a: bool, b: bool) -> bool {
            if a {
                return true;
            }
            a || b
        }

        fn label(code: char) -> &'static str {
            match code {
                'C' => "call",
                'P' => "put",
                _ => "",
            }
        }
        ''')
        self.assertEqual(['is_call', 'either', 'label'],
                         [f.name for f in result])
        self.assertEqual([1, 3, 3],
                         [f.cyclomatic_complexity for f in result])

    def test_escaped_char_literals_lifetimes_and_labels(self):
        result = get_rust_function_list(r'''
        fn scan<'a>(text: &'a str) -> usize {
            let mut n = 0;
            'outer: for c in text.chars() {
                if c == '\'' || c == '\\' || c == '\n' {
                    break 'outer;
                }
                if c == '\x41' || c == '\u{1F600}' {
                    n += 1;
                }
            }
            n
        }

        fn after() {}
        ''')
        self.assertEqual(['scan', 'after'], [f.name for f in result])
        self.assertEqual(7, result[0].cyclomatic_complexity)

    def test_closure_without_parameters_is_not_a_logical_or(self):
        result = get_rust_function_list('''
        fn closures(x: Option<i32>) -> i32 {
            let zero = || 0;
            let task = move || x;
            let fs = [|| 1, || 2];
            x.unwrap_or_else(|| zero())
        }
        ''')
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_logical_or_after_an_expression_is_counted(self):
        result = get_rust_function_list('''
        fn either(a: bool, b: Option<i32>, c: &[i32]) -> bool {
            a || b.is_some() || c[0] > 1 || (a) || true
        }
        ''')
        self.assertEqual(5, result[0].cyclomatic_complexity)

    def test_nested_block_comment_keeps_following_functions(self):
        # Rust block comments nest. A path like `src/**/foo.rs` contains `*/`,
        # so ending the comment there used to parse the rest as code and could
        # hide later functions (ripgrep globset docs: 86 → 0 on master).
        result = get_rust_function_list('''
        /*! Paths `src/**/foo.rs`.
         Nesting `{...}` is not currently allowed.
         `[ab]` matches `a` or `b`.
         `[!ab]` to match any character except for `a` and `b`.
         Metacharacters `*` and `?` can be escaped with character class.
         A `GlobBuilder` configures match semantics.
        */
        fn real() { if true {} }
        fn also() {}
        ''')
        self.assertEqual(
            [('real', 9, 9, 2), ('also', 10, 10, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_nested_block_comment_hides_inner_fn_examples(self):
        result = get_rust_function_list('''
        /*!
        See `src/**/foo.rs` for:
          fn example() { }
        */
        pub fn real() {}
        ''')
        self.assertEqual(
            [('real', 6, 6)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_empty_closure_has_twelve_tokens(self):
        function = get_rust_function_list('fn run() { let f = || 1; }')[0]
        self.assertEqual(12, function.token_count)
        self.assertEqual(1, function.cyclomatic_complexity)

    def test_closure_opener_token_counts(self):
        for expression, token_count in (
                ('move || 1', 13),
                ('async || 1', 13),
                ('/* comment */ || 1', 12),
                ('|| || 1', 13),
                ('|x| x', 14)):
            with self.subTest(expression=expression):
                function = get_rust_function_list(
                    'fn run() { let f = %s; }' % expression)[0]
                self.assertEqual(token_count, function.token_count)
                self.assertEqual(1, function.cyclomatic_complexity)

    def test_empty_closure_does_not_add_punctuation_to_wordcount(self):
        analyzer = FileAnalyzer(get_extensions(['wordcount']))
        result = analyzer.analyze_source_code(
            'a.rs', 'fn run() { let f = || 1; }')
        self.assertEqual(
            {'fn': 1, 'run': 1, 'let': 1, 'f': 1, '1': 1},
            result.wordCount)

    def test_empty_closure_is_one_halstead_operator(self):
        analyzer = FileAnalyzer(get_extensions(['halstead']))
        function = analyzer.analyze_source_code(
            'a.rs', 'fn run() { let f = || 1; }').function_list[0]
        # ( ) { let = closure-opener ; } are eight operator occurrences.
        self.assertEqual(8, function.halstead_N1)

    def test_closure_keeps_following_function_metrics(self):
        result = get_rust_function_list(
            'fn run() { let f = || { if a || b { 1 } else { 0 } }; }\n'
            'fn next(a: bool) { if a { done(); } }\n')
        self.assertEqual(
            [('run', 1, 1, 24, 3), ('next', 2, 2, 17, 2)],
            [(f.name, f.start_line, f.end_line, f.token_count,
              f.cyclomatic_complexity) for f in result])

    def test_quote_interpolation_keeps_the_closing_brace(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;
        use quote::quote;

        fn wrap(value: TokenStream) -> TokenStream {
            quote! { #value }
        }

        fn empty() -> TokenStream {
            quote! {}
        }
        ''')
        self.assertEqual(
            [('wrap', 4, 6, 1), ('empty', 8, 10, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_quote_keeps_raw_string_text_out_of_complexity(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;
        use quote::quote;

        fn template() -> TokenStream {
            quote! {
                writer.push_str(r#"one " if flag {} fn example() {} "#);
            }
        }

        fn empty() -> TokenStream {
            quote! {}
        }
        ''')
        self.assertEqual(
            [('template', 4, 8, 1), ('empty', 10, 12, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_quoted_condition_does_not_add_complexity(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;

        fn checks(value: TokenStream) -> TokenStream {
            quote::quote! {
                #value; if condition {}
            }
        }
        ''')
        self.assertEqual(
            [('checks', 3, 7, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_quoted_function_does_not_create_a_report(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;

        fn declarations(value: TokenStream) -> TokenStream {
            quote::quote! {
                #value fn generated() {}
            }
        }

        fn after() {}
        ''')
        self.assertEqual(
            [('declarations', 3, 7), ('after', 9, 9)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_quote_raw_string_keeps_the_later_delimiters(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;
        use quote::quote;

        fn template(value: TokenStream) -> TokenStream {
            quote! { { let label = r#"text " { "#; #value } }
        }

        fn after() {}
        ''')
        self.assertEqual(
            [('template', 4, 6), ('after', 8, 8)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_raw_c_spelling_outside_quote_does_not_hide_function(self):
        for prefix in (
                '// Rust C literals can begin with cr#"',
                'const RAW_C_PREFIX: &str = r##"cr#""##;',
                'const LABEL: &std::ffi::CStr = cr#"label"#;'):
            with self.subTest(prefix=prefix):
                result = get_rust_function_list(prefix + '''
        use proc_macro2::TokenStream;
        use quote::quote;

        fn emit(value: TokenStream) -> TokenStream {
            quote! { /* cr#" in a comment */ #value }
        }

        fn after() {}
        ''')
                self.assertEqual(
                    [('emit', 5, 7), ('after', 9, 9)],
                    [(f.name, f.start_line, f.end_line) for f in result])

    def test_raw_c_fallback_is_local_to_the_macro_with_the_literal(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;
        use quote::quote;

        fn retained(value: TokenStream) -> TokenStream {
            quote! {
                let banner = cr#"text " { "#;
                #value
            }
        }

        fn recovered(value: TokenStream) -> TokenStream {
            quote! { #value }
        }
        ''')
        self.assertEqual(
            [('retained', 4, 9), ('recovered', 11, 13)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_local_quote_macro_can_execute_its_argument(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;
        use quote::quote;

        fn choose(flag: bool) -> usize {
            macro_rules! quote { ($body:expr) => { $body }; }
            quote!(if flag { 1 } else { 0 })
        }

        fn emit(value: TokenStream) -> TokenStream {
            quote! { #value }
        }
        ''')
        self.assertEqual(
            [('choose', 4, 7, 2), ('emit', 9, 11, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_unresolved_quote_invocations_keep_executable_conditions(self):
        for source in (
                '''macro_rules! evaluate { ($body:expr) => { $body }; }
                use evaluate as quote;
                fn choose(flag: bool) -> usize { quote!(if flag { 1 } else { 0 }) }
                ''',
                '''#[macro_use]
                mod local {
                    macro_rules! quote { ($body:expr) => { $body }; }
                }
                fn choose(flag: bool) -> usize { quote!(if flag { 1 } else { 0 }) }
                ''',
                '''macro_rules! define_chooser {
                    ($quote:ident) => {
                        fn choose(flag: bool) -> usize { $quote!(if flag { 1 } else { 0 }) }
                    };
                }
                macro_rules! eval { ($body:expr) => { $body }; }
                define_chooser!(eval);
                '''):
            with self.subTest(source=source):
                result = get_rust_function_list(source)
                self.assertEqual(
                    [('choose', 2)],
                    [(f.name, f.cyclomatic_complexity) for f in result])

    def test_feature_selected_quote_alias_preserves_executable_metrics(self):
        evaluate = '''#[cfg(feature = "proc-macro")]
        use evaluate as render;
        '''
        interpolate = '''#[cfg(not(feature = "proc-macro"))]
        use quote::quote as render;
        '''
        calls = '''
        #[cfg(feature = "proc-macro")]
        fn choose(flag: bool) -> usize {
            render!(if flag { 1 } else { 0 })
        }

        #[cfg(not(feature = "proc-macro"))]
        fn emit(value: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
            render! { #value }
        }
        '''
        analyzer = FileAnalyzer(get_extensions(['ND', 'cognitive']))
        for items in ((evaluate, interpolate, calls), (interpolate, evaluate, calls),
                      (interpolate, calls, evaluate)):
            with self.subTest(items=items):
                source = 'macro_rules! evaluate { ($value:expr) => { $value }; }\n' + ''.join(items)
                functions = analyzer.analyze_source_code('feature_alias.rs', source).function_list
                self.assertEqual(
                    [('choose', 2, 1, 2), ('emit', 1, 0, 0)],
                    [(f.name, f.cyclomatic_complexity, f.max_nesting_depth,
                      f.cognitive_complexity) for f in functions])

    def test_quote_import_after_the_call_is_visible(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;

        fn emit(value: TokenStream) -> TokenStream {
            quote! { #value fn generated() {} }
        }

        fn choose(flag: bool) -> usize {
            macro_rules! evaluate { ($body:expr) => { $body }; }
            use evaluate as quote;
            quote!(if flag { 1 } else { 0 })
        }

        use quote::quote;
        ''')
        self.assertEqual(
            [('emit', 3, 5, 1), ('choose', 7, 11, 2)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_quote_reexport_keeps_function_delimiters(self):
        result = get_rust_function_list('''mod reexport {
            pub(crate) use quote::quote;
        }

        fn emit(value: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
            reexport::quote! { #value }
        }

        fn after() {}
        ''')
        self.assertEqual(
            [('emit', 5, 7, 1), ('after', 9, 9, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_bare_function_cast_before_quote_keeps_next_function(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;
        use quote::quote;

        fn convert(value: usize) -> TokenStream {
            let id = (|value: usize| value) as fn(_) -> _;
            let value = id(value);
            quote! { #value }
        }

        fn after() -> TokenStream { quote!(fixed) }
        ''')
        self.assertEqual(
            [('convert', 4, 8, 1), ('after', 10, 10, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_quote_with_raw_c_text_keeps_the_closing_brace(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;
        use quote::quote;

        fn emit(value: TokenStream) -> TokenStream {
            quote! { let label = cr#"default"#; #value }
        }

        fn after() {}
        ''')
        self.assertEqual(
            [('emit', 4, 6, 1), ('after', 8, 8, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_quote_in_span_expression_keeps_generated_function_out(self):
        result = get_rust_function_list('''use proc_macro2::TokenStream;
        use quote::{quote, quote_spanned};

        fn emit(value: TokenStream) -> TokenStream {
            quote_spanned!(quote!(fn generated() { if ready {} })
                .into_iter().next().unwrap().span()=> #value)
        }

        fn after() {}
        ''')
        self.assertEqual(
            [('emit', 4, 7, 1), ('after', 9, 9, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_quote_spanned_counts_the_span_expression(self):
        result = get_rust_function_list('''use proc_macro2::{Span, TokenStream};
        use quote::quote_spanned;

        fn annotate(value: TokenStream, cached: bool) -> TokenStream {
            quote_spanned! {
                if cached { Span::call_site() } else { Span::mixed_site() } =>
                #value; if condition {}
            }
        }
        ''')
        self.assertEqual(
            [('annotate', 4, 9, 2)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])
