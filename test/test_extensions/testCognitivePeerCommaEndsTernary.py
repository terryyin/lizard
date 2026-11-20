"""Peer ``,`` ends an open ternary so following ternaries stay siblings.

Brace languages share one counter: a comma at the ternary's bracket depth
pops that frame. Nested ternaries (deeper brackets) still nest.
"""
import unittest

from .testCognitiveComplexity import cogc, cpp


class TestPeerCommaEndsTernary(unittest.TestCase):
    """A peer ``,`` at a ternary's bracket depth ends that ternary so the
    next one in the call, list, or equivalent is a sibling (not nested)."""

    def test_javascript_peer_argument_ternaries(self):
        self.assertEqual([2], cogc("a.js",
            "function f(a, b) { go(a ? x : b, b ? 1 : 2) }"))

    def test_javascript_template_form_peer_arguments(self):
        self.assertEqual([2], cogc("a.js",
            "function f(a, b) { go(a ? `${a}` : b, b ? 1 : 2) }"))

    def test_javascript_peer_array_element_ternaries(self):
        self.assertEqual([2], cogc("a.js",
            "function f(a, b) { const xs = [a ? 1 : 2, b ? 3 : 4]; }"))

    def test_javascript_nested_ternary_still_nests(self):
        self.assertEqual([3], cogc("a.js",
            "function f(a, b) { x = a ? (b ? 1 : 2) : 3 }"))

    def test_cpp_peer_argument_ternaries_are_siblings(self):
        self.assertEqual([2], cpp(
            "void f(int a, int b) { go(a ? 1 : 2, b ? 3 : 4); }"))
