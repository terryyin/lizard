import unittest
import subprocess
from unittest.mock import patch
import lizard
import os
import sys


@patch('lizard.md5_hash_file')
@patch('lizard.auto_read', create=True)
@patch.object(os, 'walk')
@patch.object(lizard, 'print_result')
class TestApplication(unittest.TestCase):

    def testEmptyResult(self, print_result, os_walk, mock_open, _):

        def check_empty_result(result, options, scheme, _):
            self.assertEqual([], list(result))
            return 0

        os_walk.return_value = [('.', [], [])]
        print_result.side_effect = check_empty_result
        lizard.main(['lizard'])

    def testFilesWithFunction(self, print_result, os_walk, mock_open, _):
        def check_result(result, options, scheme, _):
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
        def store_result(result, options, scheme, _):
            self.fileInfos = list(result)
            return self.returned_warning_count
        mock_open.return_value = src
        print_result.side_effect = store_result
        self.exit_code = lizard.main(argv)
        return self.fileInfos

    @patch('lizard.md5_hash_file')
    @patch.object(os, 'walk')
    def runApplicationWithArgv(self, argv, os_walk, _):
        os_walk.return_value = [('.', [], ['a.cpp'])]
        return self.run_with_mocks(argv, self.source_code)

    def test_with_preprocessor_counted_in_CCN(self):
        self.runApplicationWithArgv(['lizard'])
        self.assertEqual(7, self.fileInfos[0].function_list[0].cyclomatic_complexity)

    @patch('webbrowser.open')
    def test_using_the_WordCount_plugin(self, webopen):
        self.runApplicationWithArgv(['lizard', '-EWordCount'])
        self.assertEqual(1, self.fileInfos[0].wordCount["foo"])

    def test_using_modified_ccn(self):
        self.runApplicationWithArgv(['lizard', '--modified'])
        self.assertEqual(4, self.fileInfos[0].function_list[0].cyclomatic_complexity)

    def test_exit_code(self):
        self.returned_warning_count = 6
        self.runApplicationWithArgv(['lizard', '-C5'])
        self.assertEqual(self.exit_code, 1)
        self.runApplicationWithArgv(['lizard', '-i5'])
        self.assertEqual(self.exit_code, 1)

    def test_exit_code_ignore_warnings(self):
        self.returned_warning_count = 6
        self.runApplicationWithArgv(['lizard', '-i6'])
        self.assertEqual(self.exit_code, 0)
        self.runApplicationWithArgv(['lizard', '-i', '-1'])
        self.assertEqual(self.exit_code, 0)

    def test_lizard_script_runs_with_version(self):
        """Test lizard.py runs correctly when executed as script (issue #460)."""
        lizard_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        lizard_py = os.path.join(lizard_dir, 'lizard.py')
        result = subprocess.run(
            [sys.executable, lizard_py, '--version'],
            capture_output=True,
            text=True,
            cwd=lizard_dir,
        )
        self.assertEqual(result.returncode, 0, msg=result.stderr or result.stdout)
