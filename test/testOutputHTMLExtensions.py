"""Extension metrics on the same HTML command path as core metrics."""
from html.parser import HTMLParser
from pathlib import Path
import tempfile
import unittest

from lizard import main


class MetricTable(HTMLParser):
    def __init__(self, html):
        super().__init__()
        self.rows = []
        self.active = False
        self.cell = None
        self.feed(html)

    def handle_starttag(self, tag, attrs):
        if tag == 'table':
            self.active = dict(attrs).get('id') == 'complexityTable'
        elif self.active and tag == 'tr':
            self.rows.append([])
        elif self.active and tag in ('td', 'th'):
            self.cell = ['', dict(attrs).get('class')]

    def handle_data(self, data):
        if self.cell is not None:
            self.cell[0] += data

    def handle_endtag(self, tag):
        if tag in ('td', 'th') and self.cell is not None:
            self.rows[-1].append((' '.join(self.cell[0].split()), self.cell[1]))
            self.cell = None
        elif tag == 'table':
            self.active = False


class TestHTMLExtensionMetrics(unittest.TestCase):
    def report(self, source, *options, suffix='.py'):
        with tempfile.TemporaryDirectory() as directory:
            filename = Path(directory) / ('example' + suffix)
            report = Path(directory) / 'report.html'
            filename.write_text(source, encoding='utf8')
            status = main(['lizard', str(filename), *options, '-o', str(report)])
            html = report.read_text(encoding='utf8')
        return status, MetricTable(html).rows, html

    def test_halstead_properties_are_rendered_with_their_captions(self):
        status, rows, html = self.report('def add(a, b):\n    return a + b\n', '-Ehalstead')
        self.assertEqual(0, status)
        headers = [text for text, _ in rows[0]]
        self.assertEqual(len(headers), len(rows[1]))
        for caption in ('H-volume', 'H-diff', 'H-effort'):
            self.assertIn(caption, headers)
            value, style = rows[1][headers.index(caption)]
            self.assertGreater(float(value), 0)
            self.assertEqual('value', style)
        self.assertIn('"targets": [2, 3, 4, 5, 6, 7, 8]', html)

    def test_extension_threshold_is_shown_and_highlighted(self):
        status, rows, _ = self.report(
            'def branch(x):\n    if x:\n        return 1\n',
            '-Ecognitive', '-T', 'cognitive_complexity=0')
        self.assertEqual(1, status)
        headers = [text for text, _ in rows[0]]
        self.assertIn('CogC (0)', headers)
        self.assertEqual(('1', 'greater-value'), rows[1][headers.index('CogC (0)')])

    def test_inherited_zero_metric_is_not_a_blank_cell(self):
        status, rows, _ = self.report('sub forward;', '-Ecognitive', suffix='.pl')
        self.assertEqual(0, status)
        headers = [text for text, _ in rows[0]]
        self.assertEqual(('0', 'lesser-value'), rows[1][headers.index('CogC (15)')])

    def test_core_report_keeps_its_existing_columns(self):
        status, rows, _ = self.report('def empty():\n    pass\n')
        self.assertEqual(0, status)
        self.assertEqual(6, len(rows[0]))
        self.assertEqual(6, len(rows[1]))
