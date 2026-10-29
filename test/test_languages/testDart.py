import os
import subprocess
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

import lizard

from .dart_helpers import get_dart_function_list


class TestDartFunctionRecognition(unittest.TestCase):

    def test_greet_branching_and_parameters(self):
        result = get_dart_function_list('''
String greet(String name, {required int times, bool shout = false}) {
  var s = 'Hello $name ${shout ? "!" : "."}';
  if (times > 1 && shout) {
    s = s.toUpperCase();
  }
  return s;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('greet', result[0].name)
        self.assertEqual(3, result[0].cyclomatic_complexity)
        self.assertEqual(3, result[0].parameter_count)
        self.assertEqual(7, result[0].nloc)

    def test_simple_add(self):
        result = get_dart_function_list('''
int add(int a, int b) {
  return a + b;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('add', result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_widget_build_closures_counted_in_enclosing(self):
        result = get_dart_function_list('''
class MyWidget {
  Widget build(BuildContext context) {
    return ElevatedButton(onPressed: () {
      if (enabled) {
        run();
      }
    }, child: items.isEmpty ? Text('none') :
      Text('${items.length}'));
  }
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('MyWidget::build', result[0].name)
        self.assertEqual(3, result[0].cyclomatic_complexity)
        self.assertFalse(any('(anonymous)' in f.name for f in result))

    def test_mixin_method_is_qualified(self):
        result = get_dart_function_list('''
mixin M {
  int foo() {
    return 1;
  }
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('M::foo', result[0].name)

    def test_enum_method_is_qualified(self):
        result = get_dart_function_list('''
enum E {
  a,
  b;
  int bar() {
    return 2;
  }
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('E::bar', result[0].name)

    def test_extension_method_stays_unqualified(self):
        result = get_dart_function_list('''
extension on int {
  int twice() {
    return this * 2;
  }
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('twice', result[0].name)


class TestDartNullSafetyOperators(unittest.TestCase):

    def test_null_coalescing_counts_once(self):
        result = get_dart_function_list('''
int f(int? x) {
  return x ?? 0;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_null_aware_access_and_cascade_and_bang_add_nothing(self):
        result = get_dart_function_list('''
int f(a, x) {
  var b = a?.b;
  a?..c();
  return x!;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_null_aware_assignment_counts_once(self):
        result = get_dart_function_list('''
int f(a, b) {
  a ??= b;
  return a;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_nullable_type_in_generic_adds_nothing(self):
        result = get_dart_function_list('''
int f() {
  List<int?> xs;
  return xs.length;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_spaced_ternary_still_counts(self):
        result = get_dart_function_list('''
int f(x, y, z, w) {
  x = y ? z : w;
  return x;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_optional_positional_parameter_and_null_coalesce(self):
        result = get_dart_function_list('''
int f(int? x, [int fallback = 0]) {
  return x ?? fallback;
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('f', result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(2, result[0].parameter_count)


class TestDartStringsAndCommentsInert(unittest.TestCase):

    def test_raw_triple_quoted_strings_and_doc_comments_are_inert(self):
        result = get_dart_function_list("""
/// if while
int f() {
  var m = '''it's ? a : b''';
  var r = r'$x ? y : z';
  var d = \"\"\" ? \"\"\";
  return 1;
}
int g() { return 2; }
""")
        self.assertEqual(2, len(result))
        self.assertEqual('f', result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(6, result[0].nloc)
        self.assertEqual('g', result[1].name)
        self.assertEqual(1, result[1].cyclomatic_complexity)


class TestDartCliDiscovery(unittest.TestCase):

    def setUp(self):
        self.lizard_dir = os.path.dirname(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
        self.lizard_py = os.path.join(self.lizard_dir, 'lizard.py')

    def test_language_filter_and_help(self):
        with TemporaryDirectory() as directory:
            lib_dir = Path(directory) / 'lib'
            lib_dir.mkdir()
            (lib_dir / 'main.dart').write_text('''
int main() {
  return 0;
}
''')
            (Path(directory) / 'tool.py').write_text('''
def f():
    return 1
''')

            dart_only = subprocess.run(
                [sys.executable, self.lizard_py, '-l', 'dart', directory],
                capture_output=True,
                text=True,
                cwd=self.lizard_dir,
            )
            self.assertEqual(0, dart_only.returncode,
                             msg=dart_only.stderr or dart_only.stdout)
            self.assertIn('main', dart_only.stdout)
            self.assertNotIn(' f ', dart_only.stdout)
            self.assertNotIn('tool.py', dart_only.stdout)

            both = subprocess.run(
                [sys.executable, self.lizard_py, directory],
                capture_output=True,
                text=True,
                cwd=self.lizard_dir,
            )
            self.assertEqual(0, both.returncode,
                             msg=both.stderr or both.stdout)
            self.assertIn('main', both.stdout)
            self.assertIn('f', both.stdout)

        help_result = subprocess.run(
            [sys.executable, self.lizard_py, '-h'],
            capture_output=True,
            text=True,
            cwd=self.lizard_dir,
        )
        self.assertEqual(0, help_result.returncode,
                         msg=help_result.stderr or help_result.stdout)
        self.assertIn('dart', help_result.stdout)

    def test_analyze_api_discovers_dart(self):
        with TemporaryDirectory() as directory:
            dart_path = Path(directory) / 'lib' / 'main.dart'
            dart_path.parent.mkdir()
            dart_path.write_text('''
int main() {
  return 0;
}
''')
            results = list(lizard.analyze([directory]))
            names = [f.name for info in results for f in info.function_list]
            self.assertIn('main', names)
