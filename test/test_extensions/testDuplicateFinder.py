import unittest
from lizard_ext.lizardduplicate import DuplicateFinder

class TestDuplicateFinder(unittest.TestCase):

    def find_in(self, nodes):
        dups = []
        finder = DuplicateFinder(dups.append)
        for t in nodes + [-1]:
            finder.find_duplicates(t, t)
        return dups

    def test_simple_duplicate(self):
        self.assertEqual([[[1], [1]]], self.find_in([1,1]))

    def test_mulitiple_nodes_duplicate(self):
        self.assertEqual([[[1, 2], [1, 2]]], self.find_in([1,2,1,2]))

