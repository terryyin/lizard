import unittest
from lizard_ext.default_ordered_dict import DefaultOrderedDict


class TestDefaultOrderedDict(unittest.TestCase):

    def test_missing_key_with_factory(self):
        d = DefaultOrderedDict(list)
        result = d["key"]
        self.assertEqual([], result)
        self.assertIn("key", d)
        # Counter-input MUTATION: the returned value must be the one
        # stored by __missing__, not a fresh factory call.  Mutating
        # `d["key"]` returning a new list each time would survive if
        # we only checked `assertEqual([], result)`.
        result.append(1)
        self.assertEqual([1], d["key"])

    def test_missing_key_uses_given_factory_not_hardcoded(self):
        # Counter-input TYPE: factory is stored and called per-miss.
        # Locks the factory assignment against a mutation that
        # hardcodes `list()` in __missing__.
        d = DefaultOrderedDict(lambda: 42)
        self.assertEqual(42, d["x"])
        self.assertEqual(42, d["y"])

    def test_missing_key_without_factory_raises(self):
        d = DefaultOrderedDict()
        with self.assertRaises(KeyError) as ctx:
            _ = d["missing"]
        # KeyError must carry the missing key, not a generic message.
        self.assertEqual("missing", ctx.exception.args[0])

    def test_reduce_with_factory(self):
        d = DefaultOrderedDict(list)
        d["x"] = [1, 2]
        reduced = d.__reduce__()
        # Lock in the full 5-tuple shape of the reduce protocol so a
        # mutation that swaps positions or drops a field is caught.
        self.assertEqual(DefaultOrderedDict, reduced[0])
        self.assertEqual((list,), reduced[1])
        self.assertIsNone(reduced[2])
        self.assertIsNone(reduced[3])
        self.assertEqual([("x", [1, 2])], list(reduced[4]))

    def test_reduce_without_factory(self):
        d = DefaultOrderedDict()
        reduced = d.__reduce__()
        self.assertEqual(DefaultOrderedDict, reduced[0])
        self.assertEqual((), reduced[1])

    def test_preserves_insertion_order(self):
        d = DefaultOrderedDict(int)
        d["b"] = 2
        d["a"] = 1
        d["c"] = 3
        self.assertEqual(["b", "a", "c"], list(d.keys()))
        # Values must also be preserved positionally — guards against
        # a mutation that orders keys but not values.
        self.assertEqual([2, 1, 3], list(d.values()))
