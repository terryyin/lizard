import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
import os


class TestPerl(unittest.TestCase):

    def test_perl_function(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl1.pl"))
        self.assertEqual(1, len(result.function_list))
        self.assertEqual('sub_with_complexity', result.function_list[0].name)
        self.assertEqual(2, result.function_list[0].cyclomatic_complexity)

    def test_perl_package_methods(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_package.pl"))
        self.assertEqual(2, len(result.function_list))
        
        # Test first method
        method1 = next(f for f in result.function_list if f.name == 'MyPackage::my_method')
        self.assertEqual(3, method1.cyclomatic_complexity)  # 1 + 2 conditions (if, elsif)
        
        # Test second method
        method2 = next(f for f in result.function_list if f.name == 'MyPackage::another_method')
        self.assertEqual(1, method2.cyclomatic_complexity)

    def test_perl_method_attributes(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_attributes.pl"))
        self.assertEqual(3, len(result.function_list))
        
        # Test method with single attribute
        method1 = next(f for f in result.function_list if f.name == 'TestAttributes::method_with_attr')
        self.assertEqual(1, method1.cyclomatic_complexity)
        
        # Test method with multiple attributes
        method2 = next(f for f in result.function_list if f.name == 'TestAttributes::lvalue_method')
        self.assertEqual(1, method2.cyclomatic_complexity)
        
        # Test method with attribute and conditions
        method3 = next(f for f in result.function_list if f.name == 'TestAttributes::complex_method')
        self.assertEqual(3, method3.cyclomatic_complexity)  # 1 + 2 conditions (if, elsif)

    def test_perl_forgive_comment(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_forgive.pl"))
        self.assertEqual(0, len(result.function_list))  # Function should be forgiven

    def test_perl_forgive_comment_variations(self):
        test_cases = [
            "# #lizard forgives",
            "#lizard forgives",
            "##lizard forgives",
            "###lizard forgives",
            "# lizard forgives",
        ]
        
        for comment in test_cases:
            code = f"""
{comment}
sub complex_sub {{
    my ($x) = @_;
    if ($x > 0) {{
        print "Positive\\n";
    }}
    return $x;
}}
"""
            with open("test.pl", "w") as f:
                f.write(code)
            try:
                result = analyze_file("test.pl")
                self.assertEqual(0, len(result.function_list), 
                               f"Failed for comment format: {comment}")
            finally:
                if os.path.exists("test.pl"):
                    os.remove("test.pl")

    def test_perl_forgive_inside_sub(self):
        code = """
sub complex_sub {
    # #lizard forgives
    my ($x) = @_;
    if ($x > 0) {
        print "Positive\\n";
    }
    return $x;
}
"""
        with open("test.pl", "w") as f:
            f.write(code)
        try:
            result = analyze_file("test.pl")
            self.assertEqual(0, len(result.function_list))
        finally:
            if os.path.exists("test.pl"):
                os.remove("test.pl") 