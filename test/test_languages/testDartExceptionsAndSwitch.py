import unittest

from .dart_helpers import get_dart_function_list


class TestDartExceptionsAndSwitch(unittest.TestCase):

    def test_exception_clauses_count_once_each(self):
        result = get_dart_function_list('''
int f() {
  try { a(); } on FormatException { b(); } on IOException catch (e) {
    c();
  } catch (e) { d(); }
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('f', result[0].name)
        self.assertEqual(4, result[0].cyclomatic_complexity)

    def test_switch_expression_arms_in_expression_body(self):
        result = get_dart_function_list('''
String size(int n) => switch (n) {
  0 => 'zero',
  1 || 2 => 'small',
  _ => 'big'
};
''')
        self.assertEqual(1, len(result))
        self.assertEqual('size', result[0].name)
        self.assertEqual(4, result[0].cyclomatic_complexity)
        self.assertFalse(any('(anonymous)' in f.name for f in result))
        self.assertFalse(any(f.name == '*global*' for f in result))

    def test_switch_statement_cases_and_default(self):
        result = get_dart_function_list('''
String classify(int n) {
  switch (n) {
    case 0: return 'zero';
    case 1: case 2: return 'small';
    default: return 'big';
  }
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('classify', result[0].name)
        self.assertEqual(4, result[0].cyclomatic_complexity)

    def test_switch_expression_as_statement_initializer(self):
        result = get_dart_function_list('''
String size(int n) {
  var s = switch (n) {
    0 => 'zero',
    1 || 2 => 'small',
    _ => 'big'
  };
  return s;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('size', result[0].name)
        self.assertEqual(4, result[0].cyclomatic_complexity)
        self.assertFalse(any('(anonymous)' in f.name for f in result))
        self.assertFalse(any(f.name == '*global*' for f in result))
