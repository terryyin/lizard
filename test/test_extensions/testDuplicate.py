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
                .six_line_function()
                .six_line_function()
                .code
                )
        self.assertEqual(1, len(self.detector.duplicates))

    def test_two_functions_that_are_exactly_the_same_detail(self):
        self.detect(
                self.builder
                .six_line_function()
                .six_line_function()
                .code
                )
        self.assertEqual(2, len(self.detector.duplicates[0]))
        self.assertEqual(1, self.detector.duplicates[0][0].start_line)


class CFunctionBuilder(object):
    def __init__(self):
        self.code = ''

    def empty_function(self):
        self.code += '''
            void func() {
            }
        '''
        return self

    def six_line_function(self):
        self.code += '''
            void func1(int param) {
                int result, i = 0;
                for (; i < 10; i++) {
                    result += i * i;
                }
                return result;
            }
        '''
        return self

