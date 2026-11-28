import unittest
from lizard import analyze_file


class TestLanguageRegression(unittest.TestCase):
    def test_valid_source(self):
        samples = [('fun f() { val s = """quoted " if (x) {} " text"""; println(s) }\n'
          'fun g() {}',
          [('f', 1), ('g', 1)]),
         ('fun f() { val s = """line one\n'
          '" if (x) {} \\\n'
          'line three"""; println(s) }\n'
          'fun g() {}',
          [('f', 1), ('g', 1)]),
         ('fun f() { val s = """text""""; if (ok) {} }\nfun g() {}',
          [('f', 2), ('g', 1)])]
        for source, expected in samples:
            with self.subTest(source=source):
                result = analyze_file.analyze_source_code(
                    "sample.kt", source)
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                            for f in result.function_list])
