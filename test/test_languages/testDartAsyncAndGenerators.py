import unittest

from .dart_helpers import get_dart_function_list


class TestDartAsyncAndGenerators(unittest.TestCase):

    def test_async_star_generator_keeps_function_and_following(self):
        result = get_dart_function_list('''
Stream<int> gen() async* { yield 1; }
int after() => 1;
''')
        self.assertEqual(['gen', 'after'], [f.name for f in result])

    def test_sync_star_generator_is_recognized(self):
        result = get_dart_function_list('''
Iterable<int> syn() sync* { yield 1; }
''')
        self.assertEqual(1, len(result))
        self.assertEqual('syn', result[0].name)

    def test_async_block_and_arrow_bodies(self):
        result = get_dart_function_list('''
Future<int> go() async { return 1; }
Future<int> arrow() async => 1;
''')
        self.assertEqual(['go', 'arrow'], [f.name for f in result])

    def test_async_callback_inside_build_stays_one_function(self):
        result = get_dart_function_list('''
class MyWidget {
  Widget build(BuildContext context) {
    return ElevatedButton(onPressed: () async {
      if (ok) {
        await run();
      }
    }, child: Text('x'));
  }
}
''')
        self.assertEqual(1, len(result))
        self.assertEqual('MyWidget::build', result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertFalse(any('(anonymous)' in f.name for f in result))
