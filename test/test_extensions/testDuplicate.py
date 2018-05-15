import unittest
from ..testHelpers import get_cpp_fileinfo_with_extension
from lizard_ext.lizardduplicate import LizardExtension as DuplicateDetector

class TestDuplicateExtension(unittest.TestCase):

    def setUp(self):
        self.detector = DuplicateDetector()
        self.builder = CFunctionBuilder()

    def detect(self, code):
        return get_cpp_fileinfo_with_extension(code, self.detector)

    def test_empty_file(self):
        self.detect('')
        self.assertEqual([], self.detector.duplicates)

    def test_two_functions_that_are_exactly_the_same(self):
        self.detect(
                self.builder
                .six_line_function("func1")
                .six_line_function("func1")
                .code
                )
        self.assertEqual(1, len(self.detector.duplicates))

    def test_two_functions_that_are_exactly_the_same_detail(self):
        self.detect(
                self.builder
                .six_line_function("func1")
                .six_line_function("func2")
                .code
                )
        self.assertEqual(2, len(self.detector.duplicates[0]))
        self.assertEqual(1, self.detector.duplicates[0][0].start_line)
        self.assertEqual(6, self.detector.duplicates[0][0].end_line)
        self.assertEqual(7, self.detector.duplicates[0][1].start_line)
        self.assertEqual(12, self.detector.duplicates[0][1].end_line)

    def test_two_functions_that_are_not_the_same(self):
        self.detect(
                self.builder
                .six_line_function()
                .empty_function()
                .code
                )
        self.assertEqual(0, len(self.detector.duplicates))

    def test_2_duplicates(self):
        self.detect(
                self.builder
                .six_line_function()
                .six_line_function()
                .different_six_line_function()
                .different_six_line_function()
                .code
                )
        self.assertEqual(2, len(self.detector.duplicates))

    def xtest_three_functions_that_are_the_same(self):
        self.detect(
                self.builder
                .six_line_function("func1")
                .empty_function()
                .six_line_function("func2")
                .empty_function()
                .six_line_function("func3")
                .code
                )
        self.assertEqual(1, len(self.detector.duplicates))
        self.assertEqual(3, len(self.detector.duplicates[0]))



class CFunctionBuilder(object):
    def __init__(self):
        self.code = ''

    def empty_function(self):
        self.code += '''
            void func0() {
            }
        '''
        return self

    def six_line_function(self, name='func6'):
        self.code += '''
            void %s(int param) {
                int result, i = 0;
                for (; i < 10; i++) {
                    result += i * i;
                }
            }
        '''%(name)
        return self

    def different_six_line_function(self, name='func6'):
        self.code += '''
            int %s(int param, int c) {
                if (abc == def) {
                    while(param) c=c+1;
                }
                return result;
            }
        '''%(name)
        return self

