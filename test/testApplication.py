import unittest
from test.mock import patch
import lizard
from lizard import lizard_main
import os

@patch('lizard.open', create=True)
@patch.object(os, 'walk')
@patch.object(lizard, 'print_result')
class TestApplication(unittest.TestCase):
    
    def exhaust_result(self, result, options): 
        list(result)
        
    def check_empty_result(self, result, options): 
        self.assertEqual([], list(result))
        
    def testGetCurrentFolderByDefault(self, print_result, os_walk, mock_open):
        print_result.side_effect = self.exhaust_result
        lizard_main(['lizard'])
        os_walk.assert_called_once_with('.', topdown=False)

    def testEmptyResult(self, print_result, os_walk, mock_open):
        os_walk.return_value = [('.', [], [])]
        print_result.side_effect = self.check_empty_result
        lizard_main(['lizard'])

    def testFilesWithFunction(self, print_result, os_walk, mock_open):
        def check_result(result, options):
            fileInfos = list(result) 
            self.assertEqual(1, len(fileInfos))
            self.assertEqual('foo', fileInfos[0].function_list[0].name)
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.__enter__.return_value.read.return_value = "void foo(){}"
        print_result.side_effect = check_result
        lizard_main(['lizard'])

    def testMutipleFilesInArgv(self, print_result, os_walk, mock_open):
        def check_result(result, options):
            fileInfos = list(result) 
            self.assertEqual(1, len(fileInfos))
            self.assertEqual('foo', fileInfos[0].function_list[0].name)
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.__enter__.return_value.read.return_value = "void foo(){}"
        print_result.side_effect = check_result
        lizard_main(['lizard'])


class TestOptions(unittest.TestCase):

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
        
    @patch('lizard.open', create=True)
    @patch.object(os, 'walk')
    @patch.object(lizard, 'print_result')
    def runApplicationWithArgv(self, argv, print_result, os_walk, mock_open):
        def store_result(result, options):
            self.fileInfos = list(result) 
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.__enter__.return_value.read.return_value = self.source_code
        print_result.side_effect = store_result
        lizard_main(argv)
        
    def test_with_preprocessor_counted_in_CCN(self):
        self.runApplicationWithArgv(['lizard'])
        self.assertEqual(7, self.fileInfos[0].function_list[0].cyclomatic_complexity)

    def test_not_with_preprocessor_counted_in_CCN(self):
        self.runApplicationWithArgv(['lizard', '-P'])
        self.assertEqual(6, self.fileInfos[0].function_list[0].cyclomatic_complexity)
        
    def test_using_the_WordCount_plugin(self):
        self.runApplicationWithArgv(['lizard', '-EWordCount'])
        self.assertEqual(1, self.fileInfos[0].wordCount["foo"])

    def test_using_modified_ccn(self):
        self.runApplicationWithArgv(['lizard', '--modified'])
        self.assertEqual(4, self.fileInfos[0].function_list[0].cyclomatic_complexity)


from lizard import parse_args
class TestOptionParsing(unittest.TestCase):

    @patch('os.path.isfile', create=True)
    @patch('lizard.open', create=True)
    def test_load_whitelist_from_file(self, mock_open, isfile):
        isfile.return_value = True
        mock_open.return_value.read.return_value = "foo"
        _, options = parse_args(['lizard'])
        self.assertEqual("foo", options.whitelist)
        isfile.assert_called_with('whitelizard.txt')
        mock_open.assert_called_with('whitelizard.txt', mode='r')

    @patch('os.path.isfile', create=True)
    @patch('lizard.open', create=True)
    def test_should_be_empty_if_whitelist_file_doesnot_exist(self, mock_open, isfile):
        isfile.return_value = False
        _, options = parse_args(['lizard'])
        self.assertEqual("", options.whitelist)

