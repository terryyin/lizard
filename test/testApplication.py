import unittest
from mock import patch
import lizard
import os
import sys


@patch('lizard.md5_hash_file')
@patch('lizard.auto_read', create=True)
@patch.object(os, 'walk')
@patch.object(lizard, 'print_result')
class TestApplication(unittest.TestCase):

    def testEmptyResult(self, print_result, os_walk, mock_open, _):

        def check_empty_result(result, options, scheme):
            self.assertEqual([], list(result))
            return 0

        os_walk.return_value = [('.', [], [])]
        print_result.side_effect = check_empty_result
        lizard.main(['lizard'])

    def testFilesWithFunction(self, print_result, os_walk, mock_open, _):
        def check_result(result, options, scheme):
            fileInfos = list(result)
            self.assertEqual(1, len(fileInfos))
            self.assertEqual('foo', fileInfos[0].function_list[0].name)
            return 0
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value = "void foo(){}"
        print_result.side_effect = check_result
        lizard.main(['lizard'])


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
        self.returned_warning_count = 0

    @patch('lizard.auto_read', create=True)
    @patch.object(lizard, 'print_result')
    def run_with_mocks(self, argv, src, print_result, mock_open):
        def store_result(result, options, scheme):
            self.fileInfos = list(result)
            return self.returned_warning_count
        mock_open.return_value = src
        print_result.side_effect = store_result
        lizard.main(argv)
        return self.fileInfos

    @patch('lizard.md5_hash_file')
    @patch.object(os, 'walk')
    def runApplicationWithArgv(self, argv, os_walk, _):
        os_walk.return_value = [('.', [], ['a.cpp'])]
        return self.run_with_mocks(argv, self.source_code)

    def test_with_preprocessor_counted_in_CCN(self):
        self.runApplicationWithArgv(['lizard'])
        self.assertEqual(7, self.fileInfos[0].function_list[0].cyclomatic_complexity)

    def test_using_the_WordCount_plugin(self):
        self.runApplicationWithArgv(['lizard', '-EWordCount'])
        self.assertEqual(1, self.fileInfos[0].wordCount["foo"])

    def test_using_modified_ccn(self):
        self.runApplicationWithArgv(['lizard', '--modified'])
        self.assertEqual(4, self.fileInfos[0].function_list[0].cyclomatic_complexity)

    @patch.object(sys, 'exit')
    def test_exit_code(self, mock_exit):
        self.returned_warning_count = 6
        self.runApplicationWithArgv(['lizard', '-C5'])
        mock_exit.assert_called_with(1)
        self.runApplicationWithArgv(['lizard', '-i5'])
        mock_exit.assert_called_with(1)

    @patch.object(sys, 'exit')
    def test_exit_code_ignore_warnings(self, mock_exit):
        self.returned_warning_count = 6
        self.runApplicationWithArgv(['lizard', '-i6'])
        self.assertFalse(mock_exit.called)
        self.runApplicationWithArgv(['lizard', '-i', '-1'])
        self.assertFalse(mock_exit.called)
