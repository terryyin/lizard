import unittest

from lizard import analyze_file


def get_csharp_function_list(source_code):
    return analyze_file.analyze_source_code("a.cs", source_code).function_list


def reported_functions(source_code):
    return [
        (function.name, function.start_line, function.end_line)
        for function in get_csharp_function_list(source_code)
    ]


class TestCsharpNestedClass(unittest.TestCase):

    def test_method_after_nested_class_keeps_outer_name(self):
        source = """public class Outer
{
    class Inner
    {
        public int Value;
    }

    void Method()
    {
    }
}
"""
        self.assertEqual(
            [("Outer::Method", 8, 10)],
            reported_functions(source))

    def test_method_after_nested_class_with_method_keeps_outer_name(self):
        source = """public class Outer
{
    class Inner
    {
        void Inside()
        {
        }
    }

    void Method()
    {
    }
}
"""
        self.assertEqual(
            [("Inner::Inside", 5, 7), ("Outer::Method", 10, 12)],
            reported_functions(source))

    def test_method_after_property_and_nested_class_keeps_outer_name(self):
        source = """public class Outer
{
    public int X { get; set; }
    class Inner
    {
        void Inside() {}
    }
    void Method() {}
}
"""
        self.assertEqual(
            [("Inner::Inside", 6, 6), ("Outer::Method", 8, 8)],
            reported_functions(source))
