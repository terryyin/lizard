import unittest
from unittest.mock import patch
from lizard_ext.lizardboolcount import LizardExtension


class FakeFileInfo(object):
    token_count = 0


class FakeReader(object):

    class FI(object):
        token_count = 0

    def __init__(self):
        self.fileinfo = self.FI()
        self.context = self


class TestBoolCount(unittest.TestCase):

    def setUp(self):
        self.reader = FakeReader()
        self.ext = LizardExtension()

    def test_count_bool_token(self):
        list(self.ext(["bool"], self.reader))
        self.assertEqual(1, self.reader.fileinfo.bool_count)

    def test_case_insensitive_bool(self):
        list(self.ext(["BOOL", "Bool"], self.reader))
        self.assertEqual(2, self.reader.fileinfo.bool_count)

    def test_non_bool_token_not_counted(self):
        list(self.ext(["int", "void"], self.reader))
        self.assertEqual(0, self.reader.fileinfo.bool_count)

    def test_cross_file_aggregates_total(self):
        fi1 = FakeFileInfo()
        fi1.bool_count = 3
        fi1.token_count = 10
        fi2 = FakeFileInfo()
        fi2.bool_count = 2
        fi2.token_count = 20
        list(self.ext.cross_file_process([fi1, fi2]))
        self.assertEqual(5, self.ext.total_bool)
        self.assertEqual(30, self.ext.total_token)

    def test_print_result_zero_tokens(self):
        with patch('builtins.print') as mock_print:
            self.ext.print_result()
        mock_print.assert_called()

    def test_print_result_with_data(self):
        self.ext.total_bool = 10
        self.ext.total_token = 100
        with patch('builtins.print') as mock_print:
            self.ext.print_result()
        calls = [str(c) for c in mock_print.call_args_list]
        output = " ".join(calls)
        self.assertIn("10.0", output)
