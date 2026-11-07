"""Unit tests for HalsteadMetrics formulae."""

import unittest
from collections import Counter

from lizard_ext.halstead_metrics import HalsteadMetrics


class TestHalsteadMetrics(unittest.TestCase):

    def test_empty_metrics_are_zero(self):
        metrics = HalsteadMetrics()
        self.assertEqual(0, metrics.vocabulary)
        self.assertEqual(0, metrics.length)
        self.assertEqual(0.0, metrics.volume)
        self.assertEqual(0.0, metrics.difficulty)
        self.assertEqual(0.0, metrics.effort)
        self.assertEqual(0.0, metrics.calculated_length)

    def test_formulae(self):
        operators = Counter({'+': 3, '=': 2})        # n1=2, N1=5
        operands = Counter({'a': 4, 'b': 1})         # n2=2, N2=5
        metrics = HalsteadMetrics(operators, operands)
        self.assertEqual(2, metrics.distinct_operators)
        self.assertEqual(2, metrics.distinct_operands)
        self.assertEqual(5, metrics.total_operators)
        self.assertEqual(5, metrics.total_operands)
        self.assertEqual(4, metrics.vocabulary)
        self.assertEqual(10, metrics.length)
        # V = 10 * log2(4) = 20
        self.assertAlmostEqual(20.0, metrics.volume, places=6)
        # D = (2 / 2) * (5 / 2) = 2.5
        self.assertAlmostEqual(2.5, metrics.difficulty, places=6)
        # E = 2.5 * 20 = 50
        self.assertAlmostEqual(50.0, metrics.effort, places=6)
        self.assertAlmostEqual(50.0 / 18.0, metrics.time, places=6)
        self.assertAlmostEqual(20.0 / 3000.0, metrics.bugs, places=6)
