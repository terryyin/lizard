import unittest
from mock import patch, ANY
import hfcca
from hfcca import hfcca_main, FileAnalyzer
import os

class TestApplication(unittest.TestCase):
    
    def exhaust_result(self, result, options): 
        list(result)
        
    def check_empty_result(self, result, options): 
        self.assertEqual([], list(result))
        
    @patch.object(os, 'walk')
    @patch.object(hfcca, 'print_result')
    def testGetCurrentFolderByDefault(self, print_result, os_walk):
        print_result.side_effect = self.exhaust_result
        hfcca_main(['hfcca'])
        os_walk.assert_called_once_with('.', topdown=False)

    @patch.object(os, 'walk')
    @patch.object(hfcca, 'print_result')
    def testEmptyResult(self, print_result, os_walk):
        os_walk.return_value = [('.', [], [])]
        print_result.side_effect = self.check_empty_result
        hfcca_main(['hfcca'])

    @patch.object(FileAnalyzer, 'open')
    @patch.object(os, 'walk')
    @patch.object(hfcca, 'print_result')
    def testFilesWithEmptyResults(self, print_result, os_walk, mock_open):
        def check_result(result, options):
            fileInfos = list(result) 
            self.assertEqual(1, len(fileInfos))
            self.assertEqual('foo', fileInfos[0][0].name)
        os_walk.return_value = [('.', [], ['a.cpp'])]
        mock_open.return_value.read.return_value = "void foo(){}"
        print_result.side_effect = check_result
        hfcca_main(['hfcca'])



if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()