"""Raw Rust identifiers must not start a preprocessor-style token."""
import unittest

from lizard import analyze_file


class TestRustRawIdentifiers(unittest.TestCase):
    def functions(self, source):
        return analyze_file.analyze_source_code('raw.rs', source).function_list

    def test_raw_function_name_and_following_function(self):
        functions = self.functions('''fn r#match() {
    if true { return; }
}
fn after() {}
''')
        self.assertEqual([f.name for f in functions], ['r#match', 'after'])
        self.assertEqual([f.cyclomatic_complexity for f in functions], [2, 1])
        self.assertEqual([(f.start_line, f.end_line) for f in functions], [(1, 3), (4, 4)])

    def test_raw_parameter_and_local_names_keep_the_rest_of_the_line(self):
        functions = self.functions('''fn check(r#if: bool, r#type: i32) -> bool {
    let r#loop = r#type > 0;
    if r#if && r#loop { return true; }
    false
}
''')
        self.assertEqual([f.name for f in functions], ['check'])
        self.assertEqual(functions[0].parameter_count, 2)
        self.assertEqual(functions[0].cyclomatic_complexity, 3)
        self.assertEqual(functions[0].nloc, 5)

    def test_raw_member_names_are_not_control_flow(self):
        functions = self.functions('''fn check(item: Item) -> bool {
    item.r#match() || item.r#if
}
''')
        self.assertEqual([f.name for f in functions], ['check'])
        self.assertEqual(functions[0].cyclomatic_complexity, 2)
        self.assertEqual(functions[0].end_line, 3)

    def test_attributes_strings_and_comments_keep_their_boundaries(self):
        functions = self.functions('''#[allow(dead_code)]
fn normal() {
    let text = "r#if r#match";
    // r#if { r#match
    /* r#while */
    if true { return; }
}
''')
        self.assertEqual([f.name for f in functions], ['normal'])
        self.assertEqual(functions[0].cyclomatic_complexity, 2)
        self.assertEqual((functions[0].start_line, functions[0].end_line), (2, 7))
