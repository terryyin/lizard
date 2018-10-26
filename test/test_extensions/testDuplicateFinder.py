import unittest
from lizard_ext.lizardduplicate import DuplicateFinder


class Node(object):
    def __init__(self, value, hash_code):
        self.hash = hash_code
        self.value = value

class TestDuplicateFinder(unittest.TestCase):

    def find_in(self, nodes):
        nodes = list(Node("%s@%s"%(v, i), v) for i,v in enumerate(nodes + [-1]))
        finder = DuplicateFinder(nodes, [0], collapse_repeat_tokens=0)
        dups = finder.find_start_and_ends()
        dups = list(
                [(nodes[start], nodes[end]) for start, end in v]
                for v in dups)
        return [[(x.value, y.value) for x, y in n] for n in dups]

    def test_no_duplicate(self):
        self.assertEqual([], self.find_in([1,2]))

    def test_simple_duplicate(self):
        self.assertEqual([[("1@0", "1@0"), ("1@1", "1@1")]], self.find_in([1,1]))

    def test_simple_duplicate_3_times(self):
        self.assertIn([("1@0", "1@0"), ("1@1", "1@1"), ("1@2", "1@2")], self.find_in([1,1,1]))

    def test_simple_two_duplicates(self):
        self.assertIn([("1@0", "1@0"), ("1@1", "1@1")], self.find_in([1,1,2,2]))
        self.assertIn([("2@2", "2@2"), ("2@3", "2@3")], self.find_in([1,1,2,2]))

    def test_simple_3_duplicates_in_different_places(self):
        self.assertEqual([[("1@0", "1@0"), ("1@2", "1@2"), ("1@4", "1@4")]], self.find_in([1,2,1,3,1]))

    def test_multiple_nodes_duplicate(self):
        self.assertIn([("1@0", "2@1"), ("1@2", "2@3")], self.find_in([1,2,1,2]))
        self.assertNotIn([("1@0", "1@0"), ("1@2", "1@2")], self.find_in([1,2,1,2]))

    def test_not_multiple_nodes_duplicate(self):
        self.assertIn([("1@0", "1@0"),("1@3", "1@3")], self.find_in([1,3,2,1,2]))
        self.assertIn([("2@2", "2@2"),("2@4", "2@4")], self.find_in([1,3,2,1,2]))

    def test_multiple_nodes_duplicate_and_single_node(self):
        self.assertIn([("1@0", "1@0"), ("1@2", "1@2"), ("1@4", "1@4")], self.find_in([1,2,1,2,1]))
        self.assertIn([("1@0", "2@1"), ("1@2", "2@3")], self.find_in([1,2,1,2,1]))

    def test_3_nodes_duplicate(self):
        self.assertNotIn([("3@2", "3@2"), ("3@5", "3@5")], self.find_in([1,2,3,1,2,3]))

    def test_partial_different_purpose(self):
        self.assertNotIn([("2@1", "3@2"), ("2@4", "3@5")], self.find_in([1,2,3,1,2,3,2]))





