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
        # __call__ resets bool_count to 0 on entry (line 13 of
        # lizardboolcount.py).  A second run with no 'bool' tokens
        # must therefore report 0, not 1+0 accumulated — locks in the
        # per-file reset against mutation `bool_count = 0` → `= 1`.
        list(self.ext(["int", "void"], self.reader))
        self.assertEqual(0, self.reader.fileinfo.bool_count)

    def test_case_insensitive_bool(self):
        list(self.ext(["BOOL", "Bool"], self.reader))
        self.assertEqual(2, self.reader.fileinfo.bool_count)

    def test_non_bool_token_not_counted(self):
        list(self.ext(["int", "void"], self.reader))
        self.assertEqual(0, self.reader.fileinfo.bool_count)

    def test_boollike_tokens_not_counted(self):
        # Counter-input BOUNDARY + LOGIC: 'boolean', 'bool_t', '_bool'
        # are NOT exactly 'bool' (case-insensitively).  Locks in the
        # equality check against mutation `==` → `in` / `startswith`.
        list(self.ext(["boolean", "bool_t", "_bool", "Boolean"], self.reader))
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

    def test_cross_file_fileinfo_without_bool_count(self):
        # Counter-input MISSING: a fileinfo with no bool_count attr
        # (e.g., from a reader that never ran the BoolCount extension)
        # must still contribute its token_count to the total but must
        # NOT crash on the missing attribute — exercises the
        # `hasattr(fileinfo, "bool_count")` guard in cross_file_process.
        class NoBoolFileInfo(object):
            token_count = 7
        fi_ok = FakeFileInfo()
        fi_ok.bool_count = 4
        fi_ok.token_count = 13
        list(self.ext.cross_file_process([NoBoolFileInfo(), fi_ok]))
        # bool_count contribution skipped for the NoBool fileinfo
        self.assertEqual(4, self.ext.total_bool)
        # token_count accumulates for both
        self.assertEqual(20, self.ext.total_token)

    def test_print_result_zero_tokens(self):
        with patch('builtins.print') as mock_print:
            self.ext.print_result()
        mock_print.assert_called()
        # Zero-token guard: total_token is bumped to 1 to avoid
        # ZeroDivisionError (line 31 of lizardboolcount.py).  The
        # printed rate must therefore be 0.0 (0 bool / 1 token * 100).
        output = " ".join(str(c) for c in mock_print.call_args_list)
        self.assertIn("0.0", output)

    def test_print_result_with_data(self):
        self.ext.total_bool = 10
        self.ext.total_token = 100
        with patch('builtins.print') as mock_print:
            self.ext.print_result()
        calls = [str(c) for c in mock_print.call_args_list]
        output = " ".join(calls)
        # Exact rate locks the formula `bool * 100.0 / token` against
        # mutations like `/` → `*` or removed `* 100.0`.
        self.assertIn("rate %", output)
        self.assertIn("10.0", output)
        # Raw totals must also appear — locks the two plain-totals
        # print() calls against removal.
        self.assertIn("100", output)  # total_token
        self.assertIn(" 10", output)  # total_bool (leading space guards against matching "100")

    def test_print_result_non_clean_division(self):
        # Mutation-kill: ensures `/` (true division) not `//` (floor).
        # With total_bool=1, total_token=3 the rate is 100.0/3 =
        # 33.33..., whereas floor-div would give 33.0.  Pinning the
        # '33.3' substring kills the Div_FloorDiv survivor that
        # test_print_result_with_data (10/100 -> 10.0 either way)
        # did not catch.
        self.ext.total_bool = 1
        self.ext.total_token = 3
        with patch('builtins.print') as mock_print:
            self.ext.print_result()
        output = " ".join(str(c) for c in mock_print.call_args_list)
        self.assertIn("33.3", output)
        # The floor-div output would be "33.0" — must NOT appear.
        self.assertNotIn("33.0", output)
