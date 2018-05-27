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
        self.assertEqual("a.cpp", self.detector.duplicates[0][0].file_name)

    def test_two_functions_that_are_exactly_the_same_detail(self):
        self.detect(
                self.builder
                .empty_function()
                .six_line_function("func1")
                .six_line_function("func2")
                .code
                )
        self.assertEqual(2, len(self.detector.duplicates[0]))
        self.assertEqual(2, self.detector.duplicates[0][0].start_line)
        self.assertEqual(8, self.detector.duplicates[0][0].end_line)
        self.assertEqual(8, self.detector.duplicates[0][1].start_line)
        self.assertEqual(14, self.detector.duplicates[0][1].end_line)

    def test_two_5_line_functions_that_are_exactly_the_same_detail(self):
        self.detect(
                self.builder
                .empty_function()
                .five_line_function("func1")
                .five_line_function("func2")
                .code
                )
        self.assertEqual(2, len(self.detector.duplicates[0]))
        self.assertEqual(2, self.detector.duplicates[0][0].start_line)
        self.assertEqual(7, self.detector.duplicates[0][0].end_line)
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
        #this result is wrong
        self.assertEqual(3, len(self.detector.duplicates))

    def test_three_functions_that_are_the_same(self):
        self.detect(
                self.builder
                .six_line_function("func1")
                .empty_function()
                .six_line_function("func2")
                .empty_function()
                .part_of_six_line_function("func3")
                .code
                )
        self.assertEqual(2, len(self.detector.duplicates))
        self.assertEqual(3, len(self.detector.duplicates[1]))

    def test_duplicate_with_different_integer_value(self):
        self.detect(
                self.builder
                .six_line_function(number_value="123")
                .six_line_function(number_value="456")
                .code
                )
        self.assertEqual(1, len(self.detector.duplicates))

    def test_duplicate_with_different_string_value(self):
        self.detect(
                self.builder
                .six_line_function(string_value='"abc"')
                .six_line_function(string_value="'def'")
                .code
                )
        self.assertEqual(1, len(self.detector.duplicates))

    def test_duplicate_with_value_dense_block(self):
        self.detect(
                "a={" + ", ".join(str(i) for i in range(100)) + "};" +
                "b={" + ", ".join(str(i) for i in range(100, 200)) + "};"
                )
        self.assertEqual(0, len(self.detector.duplicates))

    def test_duplicate_with_different_variable_name(self):
        self.detect(
                self.builder
                .six_line_function(variable_name='abc')
                .six_line_function(variable_name="def")
                .code
                )
        self.assertEqual(1, len(self.detector.duplicates))


class CFunctionBuilder(object):
    def __init__(self):
        self.code = ''

    def empty_function(self):
        self.code += ''' void func0() {
            }
        '''
        return self

    def five_line_function(self, name='func6'):
        self.code += ''' string %s(int param, T t) {
                for (int i; i < 100; i++)
                    result += i * i;
                return i * 5 + 1015;
            }
        '''%(name)
        return self

    def six_line_function(self, name='func6', variable_name='result', number_value="10", string_value='"abc"'):
        self.code += ''' void %s(int param) {
                int %s, i = 0;
                for (; i < %s; i++) {
                     print(%s);%s += i * i;
                }
            }
        '''%(name, variable_name, number_value, string_value, variable_name)
        return self

    def part_of_six_line_function(self, name='func6'):
        self.code += ''' void %s(int param) {
                int result, i = 0;
                for (; i < 10; i++) {
                     print("abc");result += i * i;
                    then = "I do something else";
                }
            }
        '''%(name)
        return self

    def different_six_line_function(self, name='func6'):
        self.code += ''' int %s(int param, int c) {
                if (abc == def) {
                    while(param) c=c+1;
                }
                return result;
            }
        '''%(name)
        return self

