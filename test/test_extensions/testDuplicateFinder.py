import unittest
from lizard_ext.lizardduplicate import DuplicateFinder

class TestDuplicateFinder(unittest.TestCase):

    def test_simple_duplicate(self):
        dups = []
        finder = DuplicateFinder(dups.append)
        for t in [1, 1, -1]:
            finder.find_duplicates(t, t)
        self.assertEqual([[[1], [1]]], dups)

