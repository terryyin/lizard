import unittest
from mock import patch
from ..testHelpers import get_cpp_fileinfo_with_extension
from lizard_ext.lizardduplicate import LizardExtension as DuplicateDetector
from lizard import analyze_files, get_extensions


class TestDuplicateExtension(unittest.TestCase):

    def setUp(self):
        self.detector = DuplicateDetector()
        self.builder = CFunctionBuilder()

    def detect(self, code):
        self.detector.reduce(
                get_cpp_fileinfo_with_extension(code, self.detector)
            )
        return self.detector.get_duplicates()

    def test_empty_file(self):
        duplicates = self.detect('')
        self.assertEqual([], duplicates)

    def test_two_functions_that_are_exactly_the_same(self):
        duplicates = self.detect(
                self.builder
                .six_line_function("func1")
                .six_line_function("func1")
                .build()
                )
        self.assertEqual(1, len(duplicates))
        self.assertEqual("a.cpp", duplicates[0][0].file_name)

    def test_two_functions_that_are_exactly_the_same_detail(self):
        duplicates = self.detect(
                self.builder
                .empty_function()
                .six_line_function("func1")
                .six_line_function("func2")
                .build()
                )
        self.assertEqual(2, len(duplicates[0]))
        self.assertEqual(3, duplicates[0][0].start_line)
        self.assertEqual(8, duplicates[0][0].end_line)
        self.assertEqual(9, duplicates[0][1].start_line)
        self.assertEqual(14, duplicates[0][1].end_line)

    def test_two_5_line_functions_that_are_exactly_the_same_detail(self):
        duplicates = self.detect(
                self.builder
                .empty_function()
                .five_line_function("func1")
                .five_line_function("func2")
                .build()
                )
        self.assertEqual(2, len(duplicates[0]))
        self.assertEqual(3, duplicates[0][0].start_line)
        self.assertEqual(7, duplicates[0][0].end_line)
        self.assertEqual(8, duplicates[0][1].start_line)
        self.assertEqual(12, duplicates[0][1].end_line)

    def test_two_functions_that_are_not_the_same(self):
        duplicates = self.detect(
                self.builder
                .six_line_function()
                .empty_function()
                .build()
                )
        self.assertEqual(0, len(duplicates))

    def test_2_duplicates(self):
        duplicates = self.detect(
                self.builder
                .six_line_function()
                .six_line_function()
                .different_six_line_function()
                .different_six_line_function()
                .build()
                )
        #this result is wrong
        self.assertEqual(2, len(duplicates))

    def test_three_functions_that_are_the_same(self):
        duplicates = self.detect(
                self.builder
                .six_line_function("func1")
                .empty_function()
                .six_line_function("func2")
                .empty_function()
                .part_of_six_line_function("func3")
                .build()
                )
        self.assertEqual(2, len(duplicates))
        self.assertEqual(3, len(duplicates[0]))

    def test_duplicate_with_different_integer_value(self):
        duplicates = self.detect(
                self.builder
                .six_line_function(number_value="123")
                .six_line_function(number_value="456")
                .build()
                )
        self.assertEqual(1, len(duplicates))

    def test_duplicate_with_different_string_value(self):
        duplicates = self.detect(
                self.builder
                .six_line_function(string_value='"abc"')
                .six_line_function(string_value="'def'")
                .build()
                )
        self.assertEqual(1, len(duplicates))

    def test_duplicate_with_value_dense_block(self):
        duplicates = self.detect(
                "a={" + ", ".join(str(i) for i in range(100)) + "};" +
                "b={" + ", ".join(str(i) for i in range(100, 200)) + "};"
                )
        self.assertEqual(0, len(duplicates))

    def test_duplicate_with_value_dense_block_in_brackets(self):
        duplicates = self.detect(
                "a={" + ", ".join("{"+str(i)+"}" for i in range(100)) + "};" +
                "b={" + ", ".join("{"+str(i)+"}" for i in range(100, 200)) + "};"
                )
        self.assertEqual(0, len(duplicates))

    def test_duplicate_with_2_big_blocks(self):
        duplicates = self.detect(
                "a={" + ", ".join(str(i) for i in range(100)) + "};" +
                "b={" + ", ".join(str(i) for i in range(100)) + "};"
                )
        self.assertEqual(1, len(duplicates))

    def test_no_duplicate_with_1_big_blocks_of_the_same_number(self):
        duplicates = self.detect(
                "a={" + ", ".join("123\n" for i in range(100)) + "};"
                )
        self.assertEqual(0, len(duplicates))

    def test_duplicate_with_different_variable_name(self):
        duplicates = self.detect(
                self.builder
                .six_line_function(variable_name='abc')
                .six_line_function(variable_name="def")
                .build()
                )
        self.assertEqual(1, len(duplicates))

    def test_many_identifiers_together(self):
        duplicates = self.detect(
                " ".join("v"+str(i) for i in range(100))
                )
        self.assertEqual(0, len(duplicates))

    def test_repeating_patterns(self):
        duplicates = self.detect(
                self.builder
                .six_line_function()
                .six_line_function()
                .six_line_function()
                .six_line_function()
                .build()
                )
        print(duplicates)
        self.assertEqual(1, len(duplicates))



class TestDuplicateExtensionAcrossFiles(unittest.TestCase):

    def setUp(self):
        self.detector = DuplicateDetector()
        self.builder = CFunctionBuilder()

    @patch('lizard.auto_read', create=True)
    def detect(self, code1, code2, auto_read):
        auto_read.side_effect = lambda x: {'f1.cpp':code1, 'f2.cpp':code2}[x]
        extensions = get_extensions([self.detector])
        list(analyze_files(['f1.cpp', 'f2.cpp'], exts=extensions))
        return self.detector.get_duplicates()

    def test_basic_duplicate(self):
        duplicates = self.detect(
                self.builder
                .six_line_function()
                .build(),
                self.builder
                .five_line_function()
                .six_line_function()
                .build()
                )
        self.assertEqual(1, len(duplicates))
        self.assertEqual("f1.cpp", duplicates[0][0].file_name)
        self.assertEqual("f2.cpp", duplicates[0][1].file_name)
        self.assertEqual(1, duplicates[0][0].start_line)
        self.assertEqual(6, duplicates[0][0].end_line)
        self.assertEqual(6, duplicates[0][1].start_line)
        self.assertEqual(11, duplicates[0][1].end_line)


class CFunctionBuilder(object):
    def __init__(self):
        self.code = ''

    def build(self):
        code = self.code
        self.code = ''
        return code

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


