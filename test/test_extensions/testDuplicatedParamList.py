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

        @patch('lizard.auto_read', create=True)
        def run(auto_read):
            source_files = {
                'f1.cpp': FIVE_PARAM_FUNC.format(name="A"),
                'f2.cpp': FIVE_PARAM_FUNC.format(name="B"),
            }
            auto_read.side_effect = lambda filename: source_files[filename]
            extensions = get_extensions([ext])
            list(analyze_files(sorted(source_files.keys()), exts=extensions))

        run()
        self.assertEqual(2, ext.all_count_per_file["a,b,c,d,e"])

    def test_parameters_static_method(self):
        class FakeFunc(object):
            parameters = ["int a", "int b", "int c"]
        result = LizardExtension._parameters(FakeFunc())
        self.assertEqual("int a,int b,int c", result)
