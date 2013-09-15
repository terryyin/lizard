import unittest
from mock import patch
import hfcca
from hfcca import hfcca_main
import os

@patch('hfcca.open', create=True)
@patch.object(os, 'walk')
@patch.object(hfcca, 'print_result')
class TestApplication(unittest.TestCase):
    
    def exhaust_result(self, result, options): 
        list(result)
        
    def check_empty_result(self, result, options): 
        self.assertEqual([], list(result))
        
    def testGetCurrentFolderByDefault(self, print_result, os_walk, mock_open):
        print_result.side_effect = self.exhaust_result
        hfcca_main(['hfcca'])
        os_walk.assert_called_once_with('.', topdown=False)

    def testEmptyResult(self, print_result, os_walk, mock_open):
        os_walk.return_value = [('.', [], [])]
        print_result.side_effect = self.check_empty_result
        hfcca_main(['hfcca'])

    def testFilesWithFunction(self, print_result, os_walk, mock_open):
        def check_result(result, options):
            fileInfos = list(result) 
            self.assertEqual(1, len(fileInfos))
            self.assertEqual('foo', fileInfos[0].function_list[0].name)
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.__enter__.return_value.read.return_value = "void foo(){}"
        print_result.side_effect = check_result
        hfcca_main(['hfcca'])

    def testMutipleFilesInArgv(self, print_result, os_walk, mock_open):
        def check_result(result, options):
            fileInfos = list(result) 
            self.assertEqual(1, len(fileInfos))
            self.assertEqual('foo', fileInfos[0].function_list[0].name)
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.__enter__.return_value.read.return_value = "void foo(){}"
        print_result.side_effect = check_result
        hfcca_main(['hfcca'])


class TestOptions(unittest.TestCase):

    def setUp(self):
        self.source_code = '''
        void foo() {
        #if
        #endif
        }
        '''
        
    @patch('hfcca.open', create=True)
    @patch.object(os, 'walk')
    @patch.object(hfcca, 'print_result')
    def runApplicationWithArgv(self, argv, print_result, os_walk, mock_open):
        def store_result(result, options):
            self.fileInfos = list(result) 
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.__enter__.return_value.read.return_value = self.source_code
        print_result.side_effect = store_result
        hfcca_main(argv)
        
    def test_with_preprocessor_counted_in_CCN(self):
        self.runApplicationWithArgv(['hfcca'])
        self.assertEqual(2, self.fileInfos[0].function_list[0].cyclomatic_complexity)

    def test_not_with_preprocessor_counted_in_CCN(self):
        self.runApplicationWithArgv(['hfcca', '-P'])
        self.assertEqual(1, self.fileInfos[0].function_list[0].cyclomatic_complexity)


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()