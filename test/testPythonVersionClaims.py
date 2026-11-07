import re
import unittest
from pathlib import Path


class TestPythonVersionClaims(unittest.TestCase):

    def test_support_claims_match_ci_versions(self):
        root = Path(__file__).resolve().parents[1]
        workflow = (root / '.github/workflows/python-app.yml').read_text(encoding='utf-8')
        setup = (root / 'setup.py').read_text(encoding='utf-8')
        readme = (root / 'README.rst').read_text(encoding='utf-8')
        sentence = re.search(r'The test suite runs on Python (.*?)in continuous integration', readme, re.S)
        self.assertIsNotNone(sentence, 'README.rst is missing its verified Python versions sentence')

        claims = {
            'workflow matrix': set(re.findall(r'^\s*- ["\'](3\.\d+)["\']\s*$', workflow, re.M)),
            'setup.py classifiers': set(re.findall(r'Programming Language :: Python :: (3\.\d+)', setup)),
            'README.rst': set(re.findall(r'3\.\d+', sentence.group(1))),
        }
        for source, versions in claims.items():
            self.assertTrue(versions, 'No Python versions found in ' + source)

        all_versions = set.union(*claims.values())
        common_versions = set.intersection(*claims.values())
        differences = [
            '{}: missing {}; unmatched {}'.format(source, sorted(all_versions - versions),
                                                  sorted(versions - common_versions))
            for source, versions in claims.items()
        ]
        self.assertEqual(common_versions, all_versions,
                         'Python version claims disagree; update the workflow matrix, setup.py classifiers '
                         'and README.rst together.\n' + '\n'.join(differences))
