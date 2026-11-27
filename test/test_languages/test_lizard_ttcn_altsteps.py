import unittest
from lizard import analyze_file


class TestLanguageRegression(unittest.TestCase):
    def test_valid_source(self):
        samples = [('altstep handle() { [] any timer.timeout { if (ready) {} } }\n'
          'function next() {}',
          [('handle', 2), ('next', 1)]),
         ('altstep handle(integer value) runs on Component { [] any '
          'timer.timeout { if (value > 0) {} } }',
          [('handle', 2)])]
        for source, expected in samples:
            with self.subTest(source=source):
                result = analyze_file.analyze_source_code(
                    "sample.ttcn", source)
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                            for f in result.function_list])
