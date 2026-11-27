import unittest
from lizard import analyze_file


def _metrics(source):
    result = analyze_file.analyze_source_code("s.r", source)
    return [(f.name, f.start_line, f.cyclomatic_complexity)
            for f in result.function_list]


def _outer_if(raw):
    return (
        f"f <- function() {{ x <- {raw}; if (a) {{}} }}\n"
        "g <- function() { 1 }"
    )


class TestLanguageRegression(unittest.TestCase):
    def test_valid_source(self):
        samples = [
            ('f <- function() { x <- r"(text " if (x) {} " end)"; x }\n'
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
             [('f', 1), ('g', 1)]),
        ]
        for source, expected in samples:
            with self.subTest(source=source):
                result = analyze_file.analyze_source_code(
                    "sample.r", source)
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                            for f in result.function_list])

    def test_single_quote_raw_strings_hide_embedded_decisions(self):
        samples = [
            "r'(some ' if (hidden) {} ' text)'",
            "R'[some ' if (hidden) {} ' text]'",
            "r'--{some ' if (hidden) {} ' text}--'",
        ]
        for raw in samples:
            with self.subTest(raw=raw):
                self.assertEqual(
                    [('f', 1, 2), ('g', 2, 1)],
                    _metrics(_outer_if(raw)))

    def test_vertical_bar_raw_strings_hide_embedded_decisions(self):
        samples = [
            'r"|text " if (x) {} " end|"',
            "r'|some ' if (hidden) {} ' text|'",
            'r"--|text " if (x) {} " end|--"',
        ]
        for raw in samples:
            with self.subTest(raw=raw):
                self.assertEqual(
                    [('f', 1, 2), ('g', 2, 1)],
                    _metrics(_outer_if(raw)))

    def test_single_quote_multiline_raw_string_keeps_following_function_line(self):
        source = (
            "f <- function() { x <- r'-(line\n"
            "' if (x) {} '\n"
            "last)-'; if (a) {} }\n"
            "g <- function() { 1 }"
        )
        self.assertEqual([('f', 1, 2), ('g', 4, 1)], _metrics(source))

    def test_false_close_without_dashes_stays_inside_the_raw_string(self):
        self.assertEqual(
            [('f', 1, 2), ('g', 2, 1)],
            _metrics(_outer_if('r"-(a)" if (x) {} )-"')))

    def test_ordinary_string_and_identifier_r_are_not_raw_strings(self):
        ordinary = (
            'f <- function() { x <- "r\\"("; if (a) {} }\n'
            'g <- function() { 1 }'
        )
        identifier = (
            'f <- function() { r <- 1; if (r) {} }\n'
            'g <- function() { 1 }'
        )
        self.assertEqual([('f', 2), ('g', 1)], [
            (f.name, f.cyclomatic_complexity)
            for f in analyze_file.analyze_source_code("s.r", ordinary).function_list
        ])
        self.assertEqual([('f', 2), ('g', 1)], [
            (f.name, f.cyclomatic_complexity)
            for f in analyze_file.analyze_source_code("s.r", identifier).function_list
        ])
