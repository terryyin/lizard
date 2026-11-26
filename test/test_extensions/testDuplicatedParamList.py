import os
import tempfile
import unittest
from unittest.mock import patch
from ..testHelpers import get_cpp_fileinfo_with_extension
from lizard_ext.lizardduplicated_param_list import LizardExtension
from lizard import analyze_files, get_extensions


FIVE_PARAM_FUNC = '''
void func{name}(int a, int b, int c, int d, int e) {{
    return a + b + c + d + e;
}}
'''

FOUR_PARAM_FUNC = '''
void func{name}(int a, int b, int c, int d) {{
    return a + b;
}}
'''


class TestDuplicatedParamList(unittest.TestCase):

    def setUp(self):
        self.ext = LizardExtension()

    def _process(self, code):
        fileinfo = get_cpp_fileinfo_with_extension(code, self.ext)
        list(self.ext.cross_file_process([fileinfo]))
        return fileinfo

    def test_no_duplicates_with_few_params(self):
        fi = self._process(
            FOUR_PARAM_FUNC.format(name="A") +
            FOUR_PARAM_FUNC.format(name="B")
        )
        for f in fi.function_list:
            self.assertEqual(0, f.parameter_list_duplicates)

    def test_detects_duplicate_param_list(self):
        fi = self._process(
            FIVE_PARAM_FUNC.format(name="A") +
            FIVE_PARAM_FUNC.format(name="B")
        )
        duplicates = sum(f.parameter_list_duplicates for f in fi.function_list)
        self.assertGreater(duplicates, 0)

    def test_distinct_param_lists_count_self_only(self):
        # Each distinct signature appears exactly once → count == 1
        # (a function's own parameter list is always counted).
        code = '''
void funcX(int a, int b, int c, int d, int e) { return 1; }
void funcY(int p, int q, int r, int s, int t) { return 2; }
'''
        fi = self._process(code)
        for f in fi.function_list:
            self.assertEqual(1, f.parameter_list_duplicates)

    def test_cross_file_duplicate_detection(self):
        ext = LizardExtension()

        @patch('lizard_core.file_analyzer.auto_read')
        def run(auto_read):
            source_files = {
                'f1.cpp': FIVE_PARAM_FUNC.format(name="A"),
                'f2.cpp': FIVE_PARAM_FUNC.format(name="B"),
            }
            auto_read.side_effect = lambda filename: source_files[filename]
            extensions = get_extensions([ext])
            list(analyze_files(sorted(source_files.keys()), exts=extensions))

        run()
        # all_count is total occurrences (2 funcs with same signature)
        self.assertEqual(2, ext.all_count["a,b,c,d,e"])
        # all_count_per_file is de-duplicated per file (each file has
        # this signature once → 2 files)
        self.assertEqual(2, ext.all_count_per_file["a,b,c,d,e"])

    def test_below_threshold_duplicates_ignored(self):
        # Counter-input BOUNDARY: two 4-param funcs with identical
        # signatures must NOT be counted as duplicates because
        # len(parameters) < DEFAULT_MIN_PARAM_COUNT (5).  Locks the
        # `>= 5` threshold against mutation `>=` → `>` or `5` → `4`.
        fi = self._process(
            FOUR_PARAM_FUNC.format(name="A") +
            FOUR_PARAM_FUNC.format(name="B")
        )
        for f in fi.function_list:
            self.assertEqual(0, f.parameter_list_duplicates)
            self.assertEqual(0, f.parameter_list_duplicated_in_files)

    def test_parameters_static_method(self):
        class FakeFunc(object):
            parameters = ["int a", "int b", "int c"]
        result = LizardExtension._parameters(FakeFunc())
        self.assertEqual("int a,int b,int c", result)

    def test_r_cross_file_variadic_and_quoted_param_list_duplicates(self):
        ext = LizardExtension()
        files_content = {
            'f1.r': 'f1 <- function(a = list(1, 2), b = 1, c = {list(3, 4)[1]}, ..., `x y` = list(5, 6)) { a }\n',
            'f2.r': 'f2 <- function(a = c(9, 8), b = 2, c = 3, ..., `x y` = 4) { a }\n',
            'f3.r': 'f3 <- function(a = list(1, 2), b = 1, c = 2, ..., `x z` = list(5, 6)) { a }\n',
        }
        with tempfile.TemporaryDirectory() as tmpdir:
            paths = []
            for filename, content in sorted(files_content.items()):
                path = os.path.join(tmpdir, filename)
                with open(path, 'w', encoding='utf-8') as handle:
                    handle.write(content)
                paths.append(path)
            extensions = get_extensions([ext])
            fileinfos = list(analyze_files(paths, exts=extensions))

        functions = [fi.function_list[0] for fi in fileinfos]
        self.assertEqual(
            [2, 2, 1],
            [f.parameter_list_duplicates for f in functions],
        )
        self.assertEqual(
            [2, 2, 1],
            [f.parameter_list_duplicated_in_files for f in functions],
        )
