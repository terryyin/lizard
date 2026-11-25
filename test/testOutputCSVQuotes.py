"""CSV must preserve source identifiers when parsed by a CSV reader."""
import csv
from contextlib import redirect_stdout
from io import StringIO
from types import SimpleNamespace
import unittest

from lizard import AllResult, analyze_file
from lizard_ext import csv_output


class TestCSVQuotes(unittest.TestCase):
    def report(self, filename, source):
        result = analyze_file.analyze_source_code(filename, source)
        output = StringIO()
        with redirect_stdout(output):
            csv_output(AllResult([result]),
                       SimpleNamespace(extensions=[], verbose=False))
        return result.function_list[0], list(csv.reader(StringIO(output.getvalue())))

    def test_literal_operator_name_round_trips(self):
        function, rows = self.report(
            'literal.cpp',
            'long double operator "" _distance(long double value) { return value; }')
        self.assertIn('""', function.name)
        self.assertEqual(1, len(rows))
        self.assertEqual(11, len(rows[0]))
        self.assertEqual(function.name, rows[0][7])
        self.assertEqual(function.long_name, rows[0][8])
        self.assertEqual(function.name + '@1-1@literal.cpp', rows[0][5])

    def test_quoted_and_multiline_filenames_round_trip(self):
        for filename in ('source "with quotes", one.c', 'source\nwith newline.c'):
            with self.subTest(filename=filename):
                _, rows = self.report(filename, 'int plain() { return 1; }')
                self.assertEqual(1, len(rows))
                self.assertEqual(11, len(rows[0]))
                self.assertEqual(filename, rows[0][6])
                self.assertEqual('plain@1-1@' + filename, rows[0][5])
