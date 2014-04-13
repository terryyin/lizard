import unittest
from test.mock import patch
import lizard
from lizard import lizard_main
import os
import sys
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO


@patch('lizard.md5_hash_file')
@patch('lizard.open', create=True)
@patch.object(os, 'walk')
@patch.object(lizard, 'print_result')
class TestApplication(unittest.TestCase):

    @patch('lizard.md5_hash_file')
    @patch('lizard.open', create=True)
    @patch.object(os, 'walk')
    @patch.object(lizard, 'print_result')
    def check_lizard_main(self,
                          print_method,
                          print_result,
                          os_walk,
                          mock_open, _):
        print_result.side_effect = print_method
        lizard_main(['lizard'])
        os_walk.assert_called_once_with('.', topdown=False)

    def testGetCurrentFolderByDefault(self, print_result, os_walk, mock_open, _):
        def exhaust_result(result, options):
            list(result)
        self.check_lizard_main(exhaust_result)

    def testEmptyResult(self, print_result, os_walk, mock_open, _):
        def check_empty_result(result, options):
            self.assertEqual([], list(result))

        os_walk.return_value = [('.', [], [])]
        print_result.side_effect = check_empty_result
        lizard_main(['lizard'])

    def testFilesWithFunction(self, print_result, os_walk, mock_open, _):
        def check_result(result, options):
            fileInfos = list(result) 
            self.assertEqual(1, len(fileInfos))
            self.assertEqual('foo', fileInfos[0].function_list[0].name)
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.read.return_value = "void foo(){}"
        print_result.side_effect = check_result
        lizard_main(['lizard'])

    def testMutipleFilesInArgv(self, print_result, os_walk, mock_open, _):
        def check_result(result, options):
            fileInfos = list(result) 
            self.assertEqual(1, len(fileInfos))
            self.assertEqual('foo', fileInfos[0].function_list[0].name)
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.read.return_value = "void foo(){}"
        print_result.side_effect = check_result
        lizard_main(['lizard'])


class IntegrationTests(unittest.TestCase):

    def setUp(self):
        self.source_code = '''
        void foo() {
        #if
        #endif
            if(bar)
            {
            }
            switch(bar)
            {
              case 0: break;
              case 1: break;
              case 2: break;
              case 3: break;
            }
        }
        '''

    @patch('lizard.md5_hash_file')
    @patch('lizard.open', create=True)
    @patch.object(os, 'walk')
    @patch.object(lizard, 'print_result')
    def runApplicationWithArgv(self, argv, print_result, os_walk, mock_open, _):
        def store_result(result, options):
            self.fileInfos = list(result) 
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.read.return_value = self.source_code
        print_result.side_effect = store_result
        lizard_main(argv)

    def test_with_preprocessor_counted_in_CCN(self):
        self.runApplicationWithArgv(['lizard'])
        self.assertEqual(7, self.fileInfos[0].function_list[0].cyclomatic_complexity)

    def test_using_the_WordCount_plugin(self):
        self.runApplicationWithArgv(['lizard', '-EWordCount'])
        self.assertEqual(1, self.fileInfos[0].wordCount["foo"])

    def test_using_modified_ccn(self):
        self.runApplicationWithArgv(['lizard', '--modified'])
        self.assertEqual(4, self.fileInfos[0].function_list[0].cyclomatic_complexity)


from lizard import parse_args
class TestOptionParsing(unittest.TestCase):

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
