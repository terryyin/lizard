import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardio import LizardExtension as FanInOut


def fanio(*code):
    '''
    this helper function can simulate the situation that multiple files are
    processed with the io extension.
    '''
    ext = FanInOut()
    def process(ext, code):
        result = FileAnalyzer(get_extensions([ext])).analyze_source_code("a.cpp", code)
        list(ext.cross_file_process([result]))
        return result
    results = [process(ext, code) for code in code]
    return results[0].function_list[0]


class TestFanOut(unittest.TestCase):

    def fan_out(self, code):
        return fanio(code).fan_out

    def test_0_fan_out(self):
        result = self.fan_out(""" int fun(){ } """)
        self.assertEqual(0, result)

    def test_ref_to_something_not_defined(self):
        result = self.fan_out(""" int foo(){ bar;} """)
        self.assertEqual(0, result)

    def test_1_fan_out(self):
        result = self.fan_out("""
                int foo(){ bar;}
                int bar(){}
                """)
        self.assertEqual(1, result)

    def test_1_fan_out_with_2_calls_of_the_same_function(self):
        result = self.fan_out("""
                int foo(){ bar();bar();}
                int bar(){}
                """)
        self.assertEqual(1, result)

    def test_1_fan_out_with_recursive_call(self):
        result = self.fan_out("""
                void foo(){ foo();}
                """)
        self.assertEqual(1, result)

    def test_1_fan_out_with_namespace(self):
        result = self.fan_out("""
                void ns::foo(){ foo();}
                """)
        self.assertEqual(1, result)


class TestFanIn(unittest.TestCase):

    def fan_in(self, code):
        return fanio(code).fan_in

    def test_0_fan_in(self):
        result = self.fan_in(""" int fun(){ } """)
        self.assertEqual(0, result)

    def test_1_fan_in(self):
        result = self.fan_in("""
                int fun(){ }
                int bar(){ fun();}
                """)
        self.assertEqual(1, result)

    def test_1_fan_in_if_it_is_called_twice_in_the_same_function(self):
        result = self.fan_in("""
                int fun(){ }
                int bar(){ fun(); fun()}
                """)
        self.assertEqual(2, result)


class TestCombinedResult(unittest.TestCase):

    def test_1_fan_in_from_another_source_file(self):
        result = fanio(
                """ int fun(){ } """,
                """ int bar(){ fun() } """
                )
        self.assertEqual(1, result.fan_in)

    def test_1_fan_out_to_another_source_file(self):
        result = fanio(
                """ int fun(){ bar()} """,
                """ int bar(){ } """
                )
        self.assertEqual(1, result.fan_out)


class TestGeneralFanOut(unittest.TestCase):

    def general_fan_out(self, code):
        return fanio(code).general_fan_out

    def test_0_general_fan_out(self):
        result = self.general_fan_out(""" int fun(){ } """)
        self.assertEqual(0, result)

    def test_ref_to_something_not_defined(self):
        result = self.general_fan_out(""" int foo(){ bar();} """)
        self.assertEqual(1, result)

    def test_1_general_fan_out_with_space(self):
        result = self.general_fan_out("""
                int foo(){ bar   ();}
                int bar(){}
                """)
        self.assertEqual(1, result)

    def test_1_general_fan_out_multiple_punctuations(self):
        result = self.general_fan_out("""
                int foo(){ bar   ((((()));}
                int bar(){}
                """)
        self.assertEqual(1, result)

    def test_1_general_fan_out_with_2_calls_of_the_same_function(self):
        result = self.general_fan_out("""
                int foo(){ bar();bar();}
                int bar(){}
                """)
        self.assertEqual(2, result)

    def test_1_general_fan_out_with_recursive_call(self):
        result = self.general_fan_out("""
                void foo(){ foo();}
                """)
        self.assertEqual(1, result)

    def test_1_general_fan_out_with_namespace(self):
        result = self.general_fan_out("""
                void ns::foo(){ foo();}
                """)
        self.assertEqual(1, result)
