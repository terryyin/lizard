import unittest
from unittest.mock import patch
from lizard_ext.lizarddumpcomments import LizardExtension


class FakeFileInfo(object):
    filename = "test.cpp"


class FakeReader(object):

    def __init__(self, comment_map=None):
        self.fileinfo = FakeFileInfo()
        self.context = self
        self._comment_map = comment_map or {}

    def get_comment_from_token(self, token):
        return self._comment_map.get(token)


class TestDumpComments(unittest.TestCase):

    def setUp(self):
        self.ext = LizardExtension()

    def _run(self, tokens, comment_map=None):
        reader = FakeReader(comment_map)
        with patch('builtins.print') as mock_print:
            list(self.ext(tokens, reader))
        return mock_print

    def test_prints_comment(self):
        mock_print = self._run(["tok"], {"tok": "// a comment"})
        printed = [str(c) for c in mock_print.call_args_list]
        self.assertTrue(any("a comment" in s for s in printed))

    def test_skips_first_copyright_comment(self):
        mock_print = self._run(["tok"], {"tok": "// Copyright 2024"})
        printed = [str(c) for c in mock_print.call_args_list]
        self.assertFalse(any("Copyright 2024" in s for s in printed))

    def test_prints_copyright_if_not_first(self):
        mock_print = self._run(
            ["first", "second"],
            {"first": "// hello", "second": "// Copyright 2024"}
        )
        printed = [str(c) for c in mock_print.call_args_list]
        self.assertTrue(any("Copyright 2024" in s for s in printed))

    def test_yields_all_tokens(self):
        reader = FakeReader()
        with patch('builtins.print'):
            result = list(self.ext(["a", "b", "c"], reader))
        self.assertEqual(["a", "b", "c"], result)

    def test_prints_filename_header(self):
        mock_print = self._run([], {})
        # Capture the positional arg of every print() call so we can
        # check for EXACT separator strings rather than substring
        # matches.  Substring matching is too loose: a mutation
        # `"=" * 20` -> `"=" * 21` produces a 21-char string that
        # still CONTAINS a 20-char run of '=' and would survive.
        first_args = [
            c.args[0] if c.args else None
            for c in mock_print.call_args_list
        ]
        self.assertTrue(any("test.cpp" in (s or "") for s in first_args))
        # Exact-length separator assertions (kill NumberReplacer
        # mutations on the `* 20` constants).
        self.assertIn("=" * 20, first_args)
        self.assertIn("-" * 20, first_args)
        # Explicitly reject off-by-one widths that would have passed
        # a substring check.
        self.assertNotIn("=" * 21, first_args)
        self.assertNotIn("-" * 21, first_args)
        self.assertNotIn("=" * 19, first_args)
        self.assertNotIn("-" * 19, first_args)

    def test_first_flag_flips_even_when_copyright_skipped(self):
        # Counter-input LOGIC: `first = False` runs unconditionally
        # after any comment, even when the print was skipped (line 25
        # of lizarddumpcomments.py is OUTSIDE the if-skip block).  If
        # that assignment were moved inside the print branch, a file
        # starting with a copyright comment followed by a second
        # copyright comment would skip BOTH — which is wrong.
        mock_print = self._run(
            ["t1", "t2"],
            {"t1": "// Copyright 2024", "t2": "// Copyright 2025"},
        )
        printed = [str(c) for c in mock_print.call_args_list]
        # Only the first copyright is skipped; the second one must
        # appear because `first` has flipped to False.
        self.assertFalse(any("Copyright 2024" in s for s in printed))
        self.assertTrue(any("Copyright 2025" in s for s in printed))

    def test_tokens_yielded_unchanged_even_with_comments(self):
        # Counter-input MISSING: tokens must pass through verbatim
        # regardless of whether they are comments.  Locks the yield
        # against mutation `yield token` → `yield comment or token`.
        reader = FakeReader({"tok": "// hi"})
        with patch('builtins.print'):
            result = list(self.ext(["a", "tok", "b"], reader))
        self.assertEqual(["a", "tok", "b"], result)
