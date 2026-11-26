import unittest
from lizard import analyze_file


class TestLanguageRegression(unittest.TestCase):
    def test_valid_source(self):
        samples = [('f <- function() { x <- r"(text " if (x) {} " end)"; x }\n'
          'g <- function() { 1 }',
          [('f', 1), ('g', 1)]),
         ('f <- function() { x <- R"[text " if (x) {} " end]"; x }\n'
          'g <- function() { 1 }',
          [('f', 1), ('g', 1)]),
         ('f <- function() { x <- r"--{text " if (x) {} " end}--"; x }\n'
          'g <- function() { 1 }',
          [('f', 1), ('g', 1)]),
         ('f <- function() { x <- r"-(line one\n'
          '" if (x) {} "\n'
          'last)-"; x }\n'
          'g <- function() { 1 }',
          [('f', 1), ('g', 1)])]
        for source, expected in samples:
            with self.subTest(source=source):
                result = analyze_file.analyze_source_code(
                    "sample.r", source)
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                            for f in result.function_list])
