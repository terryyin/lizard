import unittest

from .dart_helpers import get_dart_function_list


class TestDartConstructors(unittest.TestCase):

    def test_point_class_story_constructors_and_members(self):
        result = get_dart_function_list('''
class Point {
  int x, y;
  Point(this.x, this.y);
  Point.origin() : x = 0, y = 0;
  factory Point.fromJson(Map<String, dynamic> json) {
    if (json['x'] == null) { return Point.origin(); }
    return Point(json['x'] as int, json['y'] as int);
  }
  int get sum => x + y;
  set scale(int f) { if (f > 0) { print(f); } }
  bool get isOrigin { return x == 0 && y == 0; }
  @override String toString() => 'Point($x, $y)';
}
''')
        names = [f.name for f in result]
        self.assertEqual(
            ['Point::fromJson', 'Point::sum', 'Point::scale=',
             'Point::isOrigin', 'Point::toString'],
            names)
        by_name = {f.name: f for f in result}
        self.assertEqual(2, by_name['Point::fromJson'].cyclomatic_complexity)
        self.assertEqual(1, by_name['Point::fromJson'].parameter_count)
        for i, f in enumerate(result[:-1]):
            self.assertLessEqual(
                f.end_line, result[i + 1].start_line,
                msg='%s spans into %s' % (f.name, result[i + 1].name))

    def test_named_constructor_with_body_and_initializing_formals(self):
        result = get_dart_function_list('''
class Point {
  Point.checked(this.x, this.y) {
    if (x < 0) { throw ArgumentError(); }
  }
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('Point::checked', result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(2, result[0].parameter_count)

    def test_unnamed_constructor_with_body(self):
        result = get_dart_function_list('''
class Point {
  Point(this.x, this.y) { assert(x >= 0); }
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('Point::Point', result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_bodyless_constructors_and_const_not_reported(self):
        result = get_dart_function_list('''
class Point {
  Point(this.x, this.y);
  Point.origin() : x = 0, y = 0;
  const Point.zero() : x = 0, y = 0;
  int get sum => x + y;
}
''')
        self.assertEqual(['Point::sum'], [f.name for f in result])
