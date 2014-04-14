import unittest
import sys
from mock import patch, Mock
from lizard import get_all_source_files
import os

class TestFilesFilter(unittest.TestCase):

    @patch.object(os, "walk")
    def test_no_matching(self, mock_os_walk):
        mock_os_walk.return_value = []
        files = get_all_source_files(["dir"], [])
        self.assertEqual(0, len(list(files)))

    @patch.object(os.path, "isfile")
    def test_explicit_file_names(self, mock_isfile):
        mock_isfile.return_value = True
        files = get_all_source_files(["dir/file.c"], [])
        self.assertEqual(["dir/file.c"], list(files))

    @patch.object(os.path, "isfile")
    def test_specific_filenames_should_not_be_excluded(self, mock_isfile):
        mock_isfile.return_value = True
        files = get_all_source_files(["dir/file.log"], [])
        self.assertEqual(["dir/file.log"], list(files))

    @patch('lizard.md5_hash_file')
    @patch.object(os, "walk")
    def test_exclude_file_name(self, mock_os_walk, md5):
        mock_os_walk.return_value = (['.', 
                                      None,
                                      ['temp.log', 'useful.cpp']],)
        files = get_all_source_files(["dir"], ["*.log"])
        self.assertEqual(["./useful.cpp"], list(files))

    @patch.object(os, "walk")
    def test_exclude_folder(self, mock_os_walk):
        mock_os_walk.return_value = (['ut', 
                                      None,
                                      ['useful.cpp']],)
        files = get_all_source_files(["dir"], ["ut/*"])
        self.assertEqual([], list(files))

    @patch.object(os, "walk")
    def test_exclude_folder_recursively(self, mock_os_walk):
        mock_os_walk.return_value = (['ut/something', 
                                      None,
                                      ['useful.cpp']],)
        files = get_all_source_files(["dir"], ["ut/*"])
        self.assertEqual([], list(files))

    @patch.object(os, "walk")
    def test_exclude_none_supported_files(self, mock_os_walk):
        mock_os_walk.return_value = (['.', 
                                      None,
                                      ['useful.txt']],)
        files = get_all_source_files(["dir"],['exclude_me'])
        self.assertEqual([], list(files))

    @patch.object(os, "walk")
    @patch("lizard.open", create=True)
    def test_duplicates(self, mock_open, mock_os_walk):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['f1.cpp', 'f2.cpp']],)
        file_handle = mock_open.return_value.__enter__.return_value
        file_handle.read.return_value = "int foo(){haha();\n}"
        files = get_all_source_files(["dir"], [])
        self.assertEqual(['./f1.cpp'], list(files))

    @patch.object(os, "walk")
    @patch("lizard.open", create=True)
    def test_nonduplicates(self, mock_open, mock_os_walk):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['f1.cpp', 'f2.cpp']],)
        file_handle = mock_open.return_value.__enter__.return_value
        outs = ["int foo(){{haha({param});\n}}".format(param=i) for i in range(2)]
        file_handle.read.side_effect = lambda: outs.pop()
        files = get_all_source_files(["dir"], [])
        self.assertEqual(["./f1.cpp", "./f2.cpp"], list(files))

    @patch.object(os, "walk")
    @patch("lizard.open", create=True)
    def test_fail_to_open_file_should_be_allowed(self, mock_open, mock_os_walk):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['f1.cpp', 'f2.cpp']],)
        file_handle = mock_open.side_effect = IOError
        files = get_all_source_files(["dir"], [])
        self.assertEqual(['./f1.cpp', './f2.cpp'], list(files))


