import subprocess
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

from lizard import analyze_file
from test.test_languages.testGherkin import SEED_EXAMPLE_1


class TestGherkinCLI(unittest.TestCase):

    def setUp(self):
        self.root = Path(__file__).resolve().parents[1]
        self.lizard_py = self.root / 'lizard.py'

    def run_lizard(self, *args, check=True):
        result = subprocess.run(
            [sys.executable, str(self.lizard_py), *args],
            capture_output=True, text=True, cwd=str(self.root))
        if check:
            self.assertEqual(
                0, result.returncode, msg=result.stderr or result.stdout)
        return result

    def test_directory_csv_and_language_filter(self):
        with TemporaryDirectory() as directory:
            path = Path(directory)
            feature = path / 'basket.feature'
            feature.write_text(SEED_EXAMPLE_1)
            (path / 'helper.py').write_text('def helper():\n    return 1\n')

            everything = self.run_lizard(str(path), '--csv')
            gherkin_only = self.run_lizard('-l', 'gherkin', str(path), '--csv')
            api = analyze_file(str(feature))

        csv_rows = [line for line in everything.stdout.splitlines() if line.strip()]
        gherkin_rows = [
            line for line in gherkin_only.stdout.splitlines() if line.strip()]
        self.assertEqual(3, len(gherkin_rows))
        self.assertEqual(3, len([
            row for row in csv_rows if 'basket.feature' in row]))
        self.assertTrue(any('helper.py' in row for row in csv_rows))
        self.assertFalse(any('helper.py' in row for row in gherkin_rows))
        for nloc in ('2,', '3,', '7,'):
            self.assertTrue(
                any(row.startswith(nloc) for row in gherkin_rows), nloc)
        self.assertTrue(any(',1,' in row for row in gherkin_rows))
        self.assertEqual(
            [2, 3, 7],
            [f.nloc for f in api.function_list])
        self.assertTrue(all(f.cyclomatic_complexity == 1 for f in api.function_list))

    def test_nloc_warning_threshold(self):
        feature = '''\
Feature: warn
  Scenario: over
    Given one
    And two
'''
        with TemporaryDirectory() as directory:
            path = Path(directory) / 'warn.feature'
            path.write_text(feature)
            over = self.run_lizard(
                str(path), '-T', 'nloc=2', '-w', check=False)
            under = self.run_lizard(
                str(path), '-T', 'nloc=3', '-w', check=False)

        self.assertNotEqual(0, over.returncode)
        self.assertIn('Scenario: over', over.stdout + over.stderr)
        self.assertEqual(0, under.returncode)
        self.assertNotIn('warning:', under.stdout + under.stderr)
