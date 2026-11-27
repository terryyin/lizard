import unittest
from lizard import analyze_file


class TestLanguageRegression(unittest.TestCase):
    def test_valid_source(self):
        samples = [('f <- \\(x) { if (x) { 1 } else { 2 } }\ng <- function() { 1 }',
          [('f', 2), ('g', 1)]),
         ('lapply(values, \\(x) { if (x) { 1 } else { 2 } })',
          [('(anonymous)', 2)])]
        for source, expected in samples:
            with self.subTest(source=source):
                result = analyze_file.analyze_source_code(
                    "sample.r", source)
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                            for f in result.function_list])
