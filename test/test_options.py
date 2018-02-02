import unittest
from lizard import parse_args
from mock import patch
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

    def test_threshold(self):
        options = parse_args(['lizard', '-Tnloc=123'])
        self.assertEqual(123, options.thresholds['nloc'])

    def test_more_threshold(self):
        options = parse_args(['lizard', '-Tnloc=123', '-T length = 300'])
        self.assertEqual(300, options.thresholds['length'])

    def test_ccn(self):
        options = parse_args(['lizard', '-C123'])
        self.assertEqual(123, options.thresholds['cyclomatic_complexity'])

    def test_length(self):
        options = parse_args(['lizard', '-L123'])
        self.assertEqual(123, options.thresholds['length'])

    def test_arguments(self):
        options = parse_args(['lizard', '-a123'])
        self.assertEqual(123, options.thresholds['parameter_count'])

    @patch.object(sys, 'exit')
    @patch('sys.stderr')
    def test_unknlown_argument_exit(self, _, mock_exit):
        options = parse_args(['lizard', '--unkown'])
        mock_exit.assert_called_with(2)

    @patch.object(sys, 'exit')
    @patch('sys.stderr')
    def test_sorting_factor_does_not_exist(self, _, mock_exit):
        options = parse_args(['lizard', '-sdoesnotexist'])
        mock_exit.assert_called_with(2)

    @patch.object(sys, 'exit')
    @patch('sys.stderr', new_callable=StringIO)
    def test_sorting_factor_does_not_exist_error_message(self, mock_stderr, mock_exit):
        options = parse_args(['lizard', '-sdoesnotexist'])
        self.assertEqual("Wrong field name 'doesnotexist'.\nCandidates are: nloc, cyclomatic_complexity, token_count, parameter_count, length, location\n", mock_stderr.getvalue())

    @patch.object(sys, 'exit')
    @patch('sys.stderr', new_callable=StringIO)
    def test_sorting_factor_does_not_exist_error_message_with_ext(self, mock_stderr, mock_exit):
        options = parse_args(['lizard', '-sdoesnotexist', '-End'])
        self.assertEqual("Wrong field name 'doesnotexist'.\nCandidates are: nloc, cyclomatic_complexity, token_count, parameter_count, length, max_nesting_depth, location\n", mock_stderr.getvalue())

    @patch.object(sys, 'exit')
    @patch('sys.stderr')
    def test_sorting_factor_does_not_exist1(self, _, mock_exit):
        options = parse_args(['lizard', '-Tdoesnotexist=3'])
        mock_exit.assert_called_with(2)

    def test_will_include_ext_args(self):
        options = parse_args(['lizard', '--ND', '2', '-End'])
        self.assertEqual(2, options.ND)
