import unittest
from test.mock import Mock, patch
from lizard_ext.lizardcpre import LizardExtension as CPreprocessor
from .testHelpers import get_cpp_function_list_with_extnesion
from lizard import CodeReader
generate_tokens = CodeReader.generate_tokens


def process_code(code, reader=None):
    reader = reader or Mock(ext=["c"])
    tokens = generate_tokens(code)
    return [t for t in CPreprocessor()(tokens, reader)]

class TestCPreprocessor(unittest.TestCase):

    def test_should_be_after_line_counter(self):
        self.assertEqual(0, CPreprocessor().ordering_index)

    def test_should_repeat_tokens(self):
        tokens = process_code("a b c")
        self.assertEqual(['a', ' ', 'b', ' ', 'c'], tokens)

    def test_should_remove_preprocessor_tokens(self):
        tokens = process_code("#blah a b c\n1")
        self.assertEqual(["\n","1"], tokens)

    def test_should_work_only_for_clike_reader(self):
        reader = Mock(ext=[])
        tokens = process_code("#\n1", reader)
        self.assertEqual(['#',"\n","1"], tokens)

    def test_should_ignore_tokens_in_else(self):
        tokens = process_code("""
        #if x
        1
        #else
        2
        #endif
        3
        """)
        self.assertNotIn("2", tokens)
        self.assertIn("3", tokens)

    def test_should_handle_nested_ifs(self):
        tokens = process_code("""
        #if x
            #if x
            1
            #else
            2
            #endif
            3
        #else
            #if x
            4
            #else
            5
            #endif
            6
        #endif
        """)
        self.assertIn("1", tokens)
        self.assertNotIn("2", tokens)
        self.assertIn("3", tokens)
        self.assertNotIn("4", tokens)
        self.assertNotIn("5", tokens)
        self.assertNotIn("6", tokens)


    def test_should_handle_nested_ifs(self):
        tokens = process_code("""
        #if x
        #else
            #if x
            #endif
        #endif
        1
        """)
        self.assertIn("1", tokens)


def analyze_with_extension(code):
    return get_cpp_function_list_with_extnesion(code, CPreprocessor())


class Test_end_2_end(unittest.TestCase):

    def test_exention_in_user(self):
        result = analyze_with_extension("#if\nxxaa#else\nint a(){}\n#endif")
        self.assertEqual(0, len(result))


