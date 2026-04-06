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
        printed = [str(c) for c in mock_print.call_args_list]
        self.assertTrue(any("test.cpp" in s for s in printed))
