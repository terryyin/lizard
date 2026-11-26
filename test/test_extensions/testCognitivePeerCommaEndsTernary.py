"""Peer ``,`` ends an open ternary so following ternaries stay siblings.

Brace languages share one counter: a comma at the ternary's bracket depth
pops that frame. Nested ternaries (deeper brackets) still nest.
"""
import unittest

import pytest

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


@pytest.mark.parametrize("extension", ["ts", "tsx"])
@pytest.mark.parametrize("result_type", ["number", "ReadonlyArray<number>"])
def test_call_type_arguments_keep_the_conditional_open(extension, result_type):
    source = f"""
        declare function make<Key, Value>(key: Key): Value;

        function select(primary: boolean, fallback: boolean) {{
            return primary ? make<string, {result_type}>("total") : fallback ? 1 : 2;
        }}
    """
    assert cogc("report." + extension, source) == [3]


def test_comparison_keeps_the_array_separator():
    source = """
        function select(primary: boolean, score: number, low: number, high: number) {
            return [primary ? 1 : score < low + 1, high > (score) ? 2 : 3];
        }
    """
    assert cogc("report.ts", source) == [2]


def test_javascript_angle_comparisons_keep_the_array_separator():
    source = """
        function select(primary, score, low, high) {
            return [primary ? 1 : score < low, high > (score) ? 2 : 3];
        }
    """
    assert cogc("report.js", source) == [2]


@pytest.mark.parametrize("extension", ["ts", "tsx"])
@pytest.mark.parametrize("lower, upper", [
    ("this.min", "this.max"),
    ('"low".length', '"high".length'),
])
def test_comparison_members_keep_the_array_separator(extension, lower, upper):
    source = f"""
        class Limits {{
            score = 1;
            min = 2;
            max = 3;

            within(preferDefault: boolean) {{
                return [preferDefault ? false : this.score < {lower},
                    {upper} > (this.score) ? 1 : 0];
            }}
        }}
    """
    assert cogc("limits." + extension, source) == [2]


def test_type_query_can_qualify_this_in_call_arguments():
    source = """
        declare function make<Key, Value>(key: Key): Value;

        class Limits {
            min = 2;

            select(primary: boolean, fallback: boolean) {
                return primary ? make<typeof this.min, number>(this.min) : fallback ? 1 : 2;
            }
        }
    """
    assert cogc("limits.ts", source) == [3]
