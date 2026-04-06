import unittest
from lizard_ext.default_ordered_dict import DefaultOrderedDict


class TestDefaultOrderedDict(unittest.TestCase):

    def test_missing_key_with_factory(self):
        d = DefaultOrderedDict(list)
        result = d["key"]
        self.assertEqual([], result)
        self.assertIn("key", d)

    def test_missing_key_without_factory_raises(self):
        d = DefaultOrderedDict()
        with self.assertRaises(KeyError):
            _ = d["missing"]

    def test_reduce_with_factory(self):
        d = DefaultOrderedDict(list)
        reduced = d.__reduce__()
        self.assertEqual((list,), reduced[1])

    def test_reduce_without_factory(self):
        d = DefaultOrderedDict()
        reduced = d.__reduce__()
        self.assertEqual((), reduced[1])

    def test_preserves_insertion_order(self):
        d = DefaultOrderedDict(int)
        d["b"] = 2
        d["a"] = 1
        d["c"] = 3
        self.assertEqual(["b", "a", "c"], list(d.keys()))
