import unittest
from lizard import parse_args
from test.mock import patch
import sys
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO


class TestOptionParsing(unittest.TestCase):

    def test_should_use_current_folder_as_default_path(self):
        options = parse_args(['lizard'])
        self.assertEqual(['.'], options.paths)

    def test_default_sorting(self):
        options = parse_args(['lizard'])
        self.assertEqual(0, len(options.sorting))

    def test_sorting_factor(self):
        options = parse_args(['lizard', '-snloc'])
        self.assertEqual("nloc", options.sorting[0])

    @patch.object(sys, 'exit')
    @patch('sys.stderr')
    def test_sorting_factor_does_not_exist(self, _, mock_exit):
        options = parse_args(['lizard', '-sdoesnotexist'])
        mock_exit.assert_called_with(2)

    @patch.object(sys, 'exit')
    @patch('sys.stderr', new_callable=StringIO)
    def test_sorting_factor_does_not_exist_error_message(self, mock_stderr, mock_exit):
        options = parse_args(['lizard', '-sdoesnotexist'])
        self.assertEqual("Wrong sorting field 'doesnotexist'.\nCandidates are: nloc, cyclomatic_complexity, token_count, parameter_count, location\n", mock_stderr.getvalue())
