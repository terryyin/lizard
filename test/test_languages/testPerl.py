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

    def test_perl_anonymous_subroutines(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_anonymous.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(5, len(result.function_list), f"Found functions: {function_names}")
        
        # Regular named function
        named_func = next(f for f in result.function_list if f.name == 'AnonymousTest::with_callback')
        self.assertEqual(1, named_func.cyclomatic_complexity)
        
        # Check simple anonymous sub
        simple_anon = next(f for f in result.function_list 
                        if f.name == 'AnonymousTest::$simple_anon')
        self.assertEqual(1, simple_anon.cyclomatic_complexity)
        
        # Check complex anonymous sub with conditionals
        complex_anon = next(f for f in result.function_list 
                         if f.name == 'AnonymousTest::$complex_anon')
        self.assertEqual(3, complex_anon.cyclomatic_complexity)  # 1 + 2 conditions (if, elsif)
        
        # Check callback anonymous sub
        callback_sub = next(f for f in result.function_list 
                          if f.name == 'AnonymousTest::$callback_sub')
        self.assertEqual(1, callback_sub.cyclomatic_complexity)
        
        # Check extra anonymous sub
        extra_anon = next(f for f in result.function_list 
                         if f.name == 'AnonymousTest::$extra_anon')
        self.assertEqual(1, extra_anon.cyclomatic_complexity)

    def test_perl_oneliners(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_oneliners.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(6, len(result.function_list), f"Found functions: {function_names}")
        
        # Functions should be properly named with their package
        simple = next(f for f in result.function_list if f.name == 'OneLinerTest::simple_oneliner')
        self.assertEqual(1, simple.cyclomatic_complexity)
        
        # Function with parameter
        param = next(f for f in result.function_list if f.name == 'OneLinerTest::param_oneliner')
        self.assertEqual(1, param.cyclomatic_complexity)
        
        # One-liner with condition should have higher complexity (1 + 2 operator characters)
        condition = next(f for f in result.function_list if f.name == 'OneLinerTest::condition_oneliner')
        self.assertEqual(3, condition.cyclomatic_complexity)  # 1 base + 1 for ? + 1 for :
        
        # Multi-statement should maintain complexity 1
        multi = next(f for f in result.function_list if f.name == 'OneLinerTest::multi_statement_oneliner')
        self.assertEqual(1, multi.cyclomatic_complexity)
        
        # Package switch should work with one-liners
        pkg = next(f for f in result.function_list if f.name == 'AnotherPackage::pkg_oneliner')
        self.assertEqual(1, pkg.cyclomatic_complexity)
        
        # Forward declaration should be recognized
        empty = next(f for f in result.function_list if f.name == 'OneLinerTest::empty_oneliner')
        self.assertEqual(1, empty.cyclomatic_complexity)

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