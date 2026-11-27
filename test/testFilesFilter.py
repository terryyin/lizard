import unittest
import platform
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch
from lizard import get_all_source_files
import os


def which_system():
    return platform.system()


def _wire_gitignore_fs_mocks(mock_exists, mock_relpath):
    '''Makes os.path.exists/relpath behave as if only a .gitignore exists
    on disk, so gitignore-matching tests don't need real filesystem access.
    '''
    def exists_side_effect(path):
        return path.endswith('.gitignore')
    mock_exists.side_effect = exists_side_effect

    def relpath_side_effect(path, start):
        if path.startswith('./'):
            path = path[2:]
        return path.replace(os.sep, '/')
    mock_relpath.side_effect = relpath_side_effect


class TestFilesFilter(unittest.TestCase):
    def test_gitignore_precedence_across_scan_roots(self):
        with TemporaryDirectory() as directory:
            root = Path(directory) / 'project'
            other = Path(directory) / 'other'
            (root / 'build').mkdir(parents=True)
            (root / 'bin' / 'Debug').mkdir(parents=True)
            other.mkdir()
            (root / '.gitignore').write_text(
                'build/\n!build/keep.py\n*.generated.py\nbin\\Debug\\\n',
                encoding='utf-8')
            kept = root / 'build' / 'keep.py'
            kept.write_text('def keep(): pass\n', encoding='utf-8')
            (root / 'build' / 'drop.py').write_text('def drop(): pass\n', encoding='utf-8')
            (root / 'bin' / 'Debug' / 'debug.py').write_text('def debug(): pass\n', encoding='utf-8')
            (other / 'app.generated.py').write_text('def generated(): pass\n', encoding='utf-8')

            files = get_all_source_files([str(root), str(other)], [], [])

            self.assertEqual([str(kept)], list(files))

    def test_gitignore_glob_compatibility(self):
        cases = [
            ('/**/generated/', {
                'generated/a.py': False,
                'src/generated/b.py': False,
                'src/keep.py': True,
            }),
            ('build**/generated.py', {
                'build-debug/generated.py': False,
                'build-debug/lib/generated.py': True,
                'generated.py': True,
            }),
            ('snapshot[[:digit:]].py', {
                'snapshot1.py': True,
                'snapshot2.py': True,
                'snapshotd].py': False,
            }),
            (r'/**/build\/**/generated.py', {
                'build/generated.py': True,
                'build/lib/generated.py': True,
                'other/build/lib/generated.py': True,
            }),
            (r'snapshot[\/[:digit:]].py', {
                'snapshot1.py': True,
                'snapshotd].py': True,
            }),
            ('[draft*.py', {
                '[draft]report.py': True,
                'report.py': True,
            }),
            ('*.py\n![draft*.py', {
                '[draft]report.py': False,
                'report.py': False,
            }),
            (r'\[draft*.py', {
                '[draft]report.py': False,
                'report.py': True,
            }),
        ]
        for pattern, files in cases:
            with self.subTest(pattern=pattern), TemporaryDirectory() as directory:
                root = Path(directory)
                (root / '.gitignore').write_text(pattern + '\n', encoding='utf-8')
                for index, relative_path in enumerate(files):
                    source_file = root / relative_path
                    source_file.parent.mkdir(parents=True, exist_ok=True)
                    source_file.write_text('value = {}\n'.format(index), encoding='utf-8')
                expected = {str(root / path) for path, keep in files.items() if keep}

                actual = set(get_all_source_files([directory], [], []))

                self.assertEqual(expected, actual)

    @patch.object(os, "walk")
    def test_no_matching(self, mock_os_walk):
        mock_os_walk.return_value = []
        files = get_all_source_files(["dir"], [], [])
        self.assertEqual(0, len(list(files)))

    @patch.object(os.path, "isfile")
    def test_explicit_file_names(self, mock_isfile):
        mock_isfile.return_value = True
        files = get_all_source_files(["dir/file.c"], [], [])
        self.assertEqual(["dir/file.c"], list(files))

    @patch.object(os.path, "isfile")
    def test_specific_filenames_should_not_be_excluded(self, mock_isfile):
        mock_isfile.return_value = True
        files = get_all_source_files(["dir/file.log"], [], [])
        self.assertEqual(["dir/file.log"], list(files))

    @patch('lizard_core.file_discovery.md5_hash_file')
    @patch.object(os, "walk")
    def test_exclude_file_name(self, mock_os_walk, md5):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['temp.c', 'useful.cpp']],)
        files = get_all_source_files(["dir"], ["*.c"], [])
        if which_system() == "Windows":
            file_names = [".\\useful.cpp"]
        else:
            file_names = ["./useful.cpp"]
        self.assertEqual(file_names, list(files))

    @patch('lizard_core.file_discovery.md5_hash_file')
    @patch.object(os, "walk")
    def test_assigned_languages(self, mock_os_walk, md5):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['temp.c', 'useful.cpp', 'x.java', 'x.js']],)
        md5.side_effect = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
        files = list(get_all_source_files(["dir"], [], ['cpp', 'java']))
        if which_system() == "Windows":
            file_names = [".\\temp.c", ".\\useful.cpp", ".\\x.java", ".\\x.js"]
        else:
            file_names = ["./temp.c", "./useful.cpp", "./x.java", "./x.js"]
        self.assertIn(file_names[0], files)
        self.assertIn(file_names[1], files)
        self.assertIn(file_names[2], files)
        self.assertNotIn(file_names[3], files)

    @patch.object(os, "walk")
    def test_exclude_folder(self, mock_os_walk):
        mock_os_walk.return_value = (['ut',
                                      None,
                                      ['useful.cpp']],)
        files = get_all_source_files(["dir"], ["ut/*"], [])
        self.assertEqual([], list(files))

    @patch.object(os, "walk")
    def test_exclude_folder_recursively(self, mock_os_walk):
        mock_os_walk.return_value = (['ut/something',
                                      None,
                                      ['useful.cpp']],)
        files = get_all_source_files(["dir"], ["ut/*"], [])
        self.assertEqual([], list(files))

    @patch.object(os, "walk")
    def test_exclude_none_supported_files(self, mock_os_walk):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['useful.txt']],)
        files = get_all_source_files(["dir"], ['exclude_me'], [])
        self.assertEqual([], list(files))

    @patch.object(os, "walk")
    @patch("lizard_core.file_discovery.auto_open", create=True)
    def test_duplicates(self, mock_open, mock_os_walk):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['f1.cpp', 'f2.cpp']],)
        file_handle = mock_open.return_value.__enter__.return_value
        file_handle.read.return_value = "int foo(){haha();\n}"
        files = get_all_source_files(["dir"], [], [])
        if which_system() == "Windows":
            file_names = [".\\f1.cpp"]
        else:
            file_names = ["./f1.cpp"]
        self.assertEqual(file_names, list(files))

    @patch.object(os, "walk")
    @patch("lizard_core.file_discovery.auto_open", create=True)
    def test_nonduplicates(self, mock_open, mock_os_walk):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['f1.cpp', 'f2.cpp']],)
        file_handle = mock_open.return_value.__enter__.return_value
        outs = ["int foo(){{haha({param});\n}}".format(param=i) for i in range(2)]
        file_handle.read.side_effect = lambda: outs.pop()
        files = get_all_source_files(["dir"], [], [])
        if which_system() == "Windows":
            file_names = [".\\f1.cpp", ".\\f2.cpp"]
        else:
            file_names = ["./f1.cpp", "./f2.cpp"]
        self.assertEqual(file_names, list(files))

    @patch.object(os, "walk")
    @patch("lizard_core.file_discovery.auto_open", create=True)
    def test_fail_to_open_file_should_be_allowed(self, mock_open, mock_os_walk):
        mock_os_walk.return_value = (['.',
                                      None,
                                      ['f1.cpp', 'f2.cpp']],)
        file_handle = mock_open.side_effect = IOError
        files = get_all_source_files(["dir"], [], [])
        if which_system() == "Windows":
            file_names = [".\\f1.cpp", ".\\f2.cpp"]
        else:
            file_names = ["./f1.cpp", "./f2.cpp"]
        self.assertEqual(file_names, list(files))

    @patch.object(os.path, "exists")
    @patch.object(os.path, "relpath")
    @patch.object(os, "walk")
    @patch("lizard_core.file_discovery.auto_read")
    def test_gitignore_filter(self, mock_auto_read, mock_os_walk, mock_relpath, mock_exists):
        mock_os_walk.return_value = (['.',
                                    None,
                                    ['temp.c', 'node_modules/file.js', 'useful.cpp']], )
        _wire_gitignore_fs_mocks(mock_exists, mock_relpath)
        mock_auto_read.return_value = "node_modules/\n*.c\n"
        
        files = get_all_source_files(["dir"], [], [])
        if which_system() == "Windows":
            file_names = [".\\useful.cpp"]
        else:
            file_names = ["./useful.cpp"]
        self.assertEqual(file_names, list(files))

    @patch.object(os.path, "exists")
    @patch.object(os.path, "relpath")
    @patch.object(os, "walk")
    @patch("lizard_core.file_discovery.auto_read")
    @patch('lizard_core.file_discovery.md5_hash_file')
    def test_gitignore_filter_can_be_disabled(self, md5, mock_auto_read,
                                              mock_os_walk, mock_relpath,
                                              mock_exists):
        mock_os_walk.return_value = (['.',
                                    None,
                                    ['temp.c', 'node_modules/file.js', 'useful.cpp']], )
        md5.side_effect = [1, 2, 3]
        mock_exists.return_value = True
        mock_relpath.side_effect = AssertionError(
            "gitignore matching should not run")
        mock_auto_read.side_effect = AssertionError(
            ".gitignore should not be read")

        files = get_all_source_files(["dir"], [], [], use_gitignore=False)
        if which_system() == "Windows":
            file_names = [".\\temp.c", ".\\node_modules/file.js", ".\\useful.cpp"]
        else:
            file_names = ["./temp.c", "./node_modules/file.js", "./useful.cpp"]
        self.assertEqual(file_names, list(files))

    @patch.object(os.path, "exists")
    @patch.object(os.path, "relpath")
    @patch.object(os, "walk")
    @patch("lizard_core.file_discovery.auto_read")
    def test_tolerates_invalid_windows_gitignore_pattern(self, mock_auto_read,
                                                         mock_os_walk,
                                                         mock_relpath,
                                                         mock_exists):
        mock_os_walk.return_value = (['.',
                                    None,
                                    ['useful.cpp']], )
        _wire_gitignore_fs_mocks(mock_exists, mock_relpath)
        mock_auto_read.return_value = "bin\\Debug\\\n"

        files = get_all_source_files(["dir"], [], [])
        if which_system() == "Windows":
            file_names = [".\\useful.cpp"]
        else:
            file_names = ["./useful.cpp"]
        self.assertEqual(file_names, list(files))

    @patch.object(os.path, "exists")
    @patch.object(os.path, "relpath")
    @patch.object(os, "walk")
    @patch("lizard_core.file_discovery.auto_read")
    def test_valid_gitignore_patterns_still_apply_with_invalid_lines(self,
                                                                     mock_auto_read,
                                                                     mock_os_walk,
                                                                     mock_relpath,
                                                                     mock_exists):
        mock_os_walk.return_value = (['.',
                                    None,
                                    ['temp.c', 'node_modules/file.js', 'useful.cpp']], )
        _wire_gitignore_fs_mocks(mock_exists, mock_relpath)
        mock_auto_read.return_value = "node_modules/\nbin\\Debug\\\n"

        files = get_all_source_files(["dir"], [], [])
        if which_system() == "Windows":
            file_names = [".\\temp.c", ".\\useful.cpp"]
        else:
            file_names = ["./temp.c", "./useful.cpp"]
        self.assertEqual(file_names, list(files))
