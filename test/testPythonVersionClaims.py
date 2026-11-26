import re
import unittest
from pathlib import Path

import yaml


class TestPythonVersionClaims(unittest.TestCase):

    def test_documented_ci_versions_cover_supported_endpoints(self):
        root = Path(__file__).resolve().parents[1]
        workflow = yaml.safe_load((root / '.github/workflows/python-app.yml').read_text(encoding='utf-8'))
        python_matrix = workflow['jobs']['build']['strategy']['matrix']['python-version']
        setup = (root / 'setup.py').read_text(encoding='utf-8')
        readme = (root / 'README.rst').read_text(encoding='utf-8')
        sentence = re.search(r'The test suite runs on Python (.*?)in continuous integration', readme, re.S)
        self.assertIsNotNone(sentence, 'README.rst is missing its verified Python versions sentence')

        tested = set(re.findall(r'3\.\d+', str(python_matrix)))
        documented = set(re.findall(r'3\.\d+', sentence.group(1)))
        supported = sorted(
            set(re.findall(r'Programming Language :: Python :: (3\.\d+)', setup)),
            key=lambda version: int(version.split('.')[1]),
        )

        self.assertTrue(tested, 'No Python versions found in the workflow matrix')
        self.assertEqual(tested, documented)
        self.assertLessEqual(tested, set(supported))
        self.assertIn(supported[0], tested)
        self.assertIn(supported[-1], tested)
