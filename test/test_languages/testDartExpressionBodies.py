import unittest

from .dart_helpers import get_dart_function_list


class TestDartExpressionBodiesAndAccessors(unittest.TestCase):

    def test_expression_body_with_null_coalesce(self):
        result = get_dart_function_list('''
int? maybe(int? x, [int fallback = 0]) => x ?? fallback;
''')
        self.assertEqual(1, len(result))
        self.assertEqual('maybe', result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(2, result[0].parameter_count)
        self.assertEqual(1, result[0].nloc)

    def test_point_getters_setters_and_arrow_tostring(self):
        result = get_dart_function_list('''
class Point {
  int x, y;
  int get sum => x + y;
  set scale(int f) { if (f > 0) { print(f); } }
  bool get isOrigin { return x == 0 && y == 0; }
  @override String toString() => 'Point($x, $y)';
}
''')
        names = [f.name for f in result]
        self.assertEqual(
            ['Point::sum', 'Point::scale=', 'Point::isOrigin', 'Point::toString'],
            names)
        by_name = {f.name: f for f in result}
        self.assertEqual(1, by_name['Point::sum'].cyclomatic_complexity)
        self.assertEqual(1, by_name['Point::sum'].nloc)
        self.assertEqual(2, by_name['Point::scale='].cyclomatic_complexity)
        self.assertEqual(1, by_name['Point::scale='].parameter_count)
        self.assertEqual(2, by_name['Point::isOrigin'].cyclomatic_complexity)
        self.assertEqual(1, by_name['Point::toString'].cyclomatic_complexity)

    def test_extension_arrow_method_stays_unqualified(self):
        result = get_dart_function_list('''
extension on int {
  int twice() => this * 2;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('twice', result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_nested_arrow_in_expression_body_stays_one_function(self):
        result = get_dart_function_list('''
int f(items) => items.map((e) => e * 2);
''')
        self.assertEqual(1, len(result))
        self.assertEqual('f', result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)
