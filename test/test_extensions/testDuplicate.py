import unittest
from ..testHelpers import get_cpp_fileinfo_with_extension
from lizard_ext.lizardduplicate import LizardExtension as DuplicateDetector
from .duplicate_c_function_builder import CFunctionBuilder


class TestDuplicateExtension(unittest.TestCase):

    def setUp(self):
        self.detector = DuplicateDetector()
        self.builder = CFunctionBuilder()

    def detect(self, code, min_duplicate_tokens = 31):
        list(self.detector.cross_file_process(
                [get_cpp_fileinfo_with_extension(code, self.detector)]
            ))
        return list(self.detector.get_duplicates(min_duplicate_tokens))

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
        self.assertEqual(1, self.detector.duplicate_rate())

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
        self.assertEqual(0, self.detector.duplicate_rate())

    def test_2_duplicates(self):
        duplicates = self.detect(
                self.builder
                .six_line_function()
                .six_line_function()
                .different_six_line_function()
                .different_six_line_function()
                .build()
                )
        self.assertEqual(2, len(duplicates))
        self.assertAlmostEqual(0.98550724637, self.detector.duplicate_rate())

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
        self.assertEqual(1, self.detector.duplicate_rate())

    def test_duplicate_with_different_string_value(self):
        duplicates = self.detect(
                self.builder
                .six_line_function(rvalue='"abc"')
                .six_line_function(rvalue="'def'")
                .build()
                )
        self.assertEqual(1, len(duplicates))

    def test_duplicate_with_different_operator(self):
        duplicates = self.detect(
                self.builder
                .six_line_function(operator='+')
                .six_line_function(operator='-')
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

    def test_identifier_after_dot_should_not_be_commonized(self):
        duplicates = self.detect(
                self.builder
                .six_line_function(rvalue="var.method()")
                .six_line_function(rvalue="var.different_method()")
                .build(),
                min_duplicate_tokens = 40
                )
        self.assertEqual(0, len(duplicates))

    def test_identifier_after_arrow_should_not_be_commonized(self):
        duplicates = self.detect(
                self.builder
                .six_line_function(rvalue="var->method()")
                .six_line_function(rvalue="var->different_method()")
                .build(),
                min_duplicate_tokens = 40
                )
        self.assertEqual(0, len(duplicates))
