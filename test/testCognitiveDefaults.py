"""Zero scores are data on every analyzed function, including declarations."""
import unittest

from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcognitive import LizardExtension


class TestCognitiveDefaults(unittest.TestCase):
    def analyze(self, filename, source):
        analyzer = FileAnalyzer(get_extensions([LizardExtension()]))
        return analyzer.analyze_source_code(filename, source).function_list

    def test_bodyless_declaration_has_an_instance_value(self):
        functions = self.analyze('example.pl', 'sub forward;')
        self.assertEqual(['forward'], [function.name for function in functions])
        self.assertEqual([0], [vars(function)['cognitive_complexity']
                               for function in functions])

    def test_declarations_and_bodies_have_consistent_data(self):
        functions = self.analyze(
            'example.pl', 'sub forward; sub empty {} sub branch { if ($x) {} }')
        self.assertEqual(
            {'forward': 0, 'empty': 0, 'branch': 1},
            {function.name: vars(function)['cognitive_complexity']
             for function in functions})

    def test_existing_zero_and_nonzero_scores_are_preserved(self):
        functions = self.analyze(
            'example.py', 'def empty():\n    pass\ndef branch(x):\n    if x:\n        return 1\n')
        self.assertEqual([0, 1], [vars(function)['cognitive_complexity']
                                  for function in functions])
