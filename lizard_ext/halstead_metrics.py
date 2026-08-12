"""Halstead derived measures from operator/operand multisets."""

import math
from collections import Counter


class HalsteadMetrics(object):
    """Compute the Halstead measures from operator/operand multisets.

    The object only stores the two :class:`collections.Counter` instances and
    derives everything on access, so it always reflects the final counts no
    matter when it is read.
    """

    def __init__(self, operators=None, operands=None):
        self._operators = operators if operators is not None else Counter()
        self._operands = operands if operands is not None else Counter()

    @property
    def distinct_operators(self):  # n1
        return len(self._operators)

    @property
    def distinct_operands(self):  # n2
        return len(self._operands)

    @property
    def total_operators(self):  # N1
        return sum(self._operators.values())

    @property
    def total_operands(self):  # N2
        return sum(self._operands.values())

    @property
    def vocabulary(self):
        """n = n1 + n2"""
        return self.distinct_operators + self.distinct_operands

    @property
    def length(self):
        """N = N1 + N2"""
        return self.total_operators + self.total_operands

    @property
    def calculated_length(self):
        """Estimated length: n1*log2(n1) + n2*log2(n2)."""
        n_1 = self.distinct_operators
        n_2 = self.distinct_operands
        return ((n_1 * math.log2(n_1) if n_1 else 0.0) +
                (n_2 * math.log2(n_2) if n_2 else 0.0))

    @property
    def volume(self):
        """V = N * log2(n)."""
        vocabulary = self.vocabulary
        return self.length * math.log2(vocabulary) if vocabulary > 0 else 0.0

    @property
    def difficulty(self):
        """D = (n1 / 2) * (N2 / n2)."""
        n_2 = self.distinct_operands
        if n_2 == 0:
            return 0.0
        return (self.distinct_operators / 2.0) * (self.total_operands / n_2)

    @property
    def effort(self):
        """E = D * V."""
        return self.difficulty * self.volume

    @property
    def time(self):
        """Estimated programming time in seconds: E / 18."""
        return self.effort / 18.0

    @property
    def bugs(self):
        """Estimated delivered bugs: V / 3000."""
        return self.volume / 3000.0
