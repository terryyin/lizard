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

    def test_perl_if_elsif_else_chains(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_control_if.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(5, len(result.function_list), f"Found functions: {function_names}")
        
        # Simple if should have complexity 2 (1 base + 1 condition)
        simple_if = next(f for f in result.function_list if f.name == 'ControlTest::simple_if')
        self.assertEqual(2, simple_if.cyclomatic_complexity)
        
        # If-else should have complexity 2 (1 base + 1 condition)
        if_else = next(f for f in result.function_list if f.name == 'ControlTest::if_else')
        self.assertEqual(2, if_else.cyclomatic_complexity)
        
        # If-elsif-else should have complexity 3 (1 base + 2 conditions)
        if_elsif_else = next(f for f in result.function_list if f.name == 'ControlTest::if_elsif_else')
        self.assertEqual(3, if_elsif_else.cyclomatic_complexity)
        
        # Multiple elsif blocks should have complexity 5 (1 base + 4 conditions)
        multi_elsif = next(f for f in result.function_list if f.name == 'ControlTest::multi_elsif')
        self.assertEqual(5, multi_elsif.cyclomatic_complexity)
        
        # Nested if statements should have complexity 4 (1 base + 3 conditions)
        nested_if = next(f for f in result.function_list if f.name == 'ControlTest::nested_if')
        self.assertEqual(4, nested_if.cyclomatic_complexity)

    def test_perl_unless_conditions(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_control_unless.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(5, len(result.function_list), f"Found functions: {function_names}")
        
        # Simple unless should have complexity 2 (1 base + 1 condition)
        simple_unless = next(f for f in result.function_list if f.name == 'UnlessTest::simple_unless')
        self.assertEqual(2, simple_unless.cyclomatic_complexity)
        
        # Unless-else should have complexity 2 (1 base + 1 condition)
        unless_else = next(f for f in result.function_list if f.name == 'UnlessTest::unless_else')
        self.assertEqual(2, unless_else.cyclomatic_complexity)
        
        # Unless with complex condition should have complexity 3 (1 base + 2 operators: && and unless)
        unless_complex = next(f for f in result.function_list if f.name == 'UnlessTest::unless_complex')
        self.assertEqual(3, unless_complex.cyclomatic_complexity)
        
        # Nested unless should have complexity 3 (1 base + 2 unless statements)
        nested_unless = next(f for f in result.function_list if f.name == 'UnlessTest::nested_unless')
        self.assertEqual(3, nested_unless.cyclomatic_complexity)
        
        # Unless with if inside should have complexity 3 (1 base + 1 unless + 1 if)
        unless_multi = next(f for f in result.function_list if f.name == 'UnlessTest::unless_multi')
        self.assertEqual(3, unless_multi.cyclomatic_complexity)

    def test_perl_while_until_loops(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_control_loops.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(6, len(result.function_list), f"Found functions: {function_names}")
        
        # Simple while should have complexity 2 (1 base + 1 condition)
        simple_while = next(f for f in result.function_list if f.name == 'LoopTest::simple_while')
        self.assertEqual(2, simple_while.cyclomatic_complexity)
        
        # While with compound condition should have complexity 3 (1 base + 2 operators: < and &&)
        while_condition = next(f for f in result.function_list if f.name == 'LoopTest::while_condition')
        self.assertEqual(3, while_condition.cyclomatic_complexity)
        
        # Nested while loops should have complexity 3 (1 base + 2 while statements)
        nested_while = next(f for f in result.function_list if f.name == 'LoopTest::nested_while')
        self.assertEqual(3, nested_while.cyclomatic_complexity)
        
        # Simple until should have complexity 2 (1 base + 1 condition)
        simple_until = next(f for f in result.function_list if f.name == 'LoopTest::simple_until')
        self.assertEqual(2, simple_until.cyclomatic_complexity)
        
        # Until with complex condition should have complexity 3 (1 base + 2 operators: >= and ||)
        until_condition = next(f for f in result.function_list if f.name == 'LoopTest::until_condition')
        self.assertEqual(3, until_condition.cyclomatic_complexity)
        
        # While with an if inside should have complexity 3 (1 base + 1 while + 1 if)
        while_with_if = next(f for f in result.function_list if f.name == 'LoopTest::while_with_if')
        self.assertEqual(3, while_with_if.cyclomatic_complexity)

    def test_perl_for_foreach_loops(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_control_for.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(7, len(result.function_list), f"Found functions: {function_names}")
        
        # C-style for loop should have complexity 2 (1 base + 1 loop condition)
        c_style_for = next(f for f in result.function_list if f.name == 'ForLoopTest::c_style_for')
        self.assertEqual(2, c_style_for.cyclomatic_complexity)
        
        # For loop with complex condition should have complexity 3 (1 base + 2 operators: < and &&)
        for_complex = next(f for f in result.function_list if f.name == 'ForLoopTest::for_complex_condition')
        self.assertEqual(3, for_complex.cyclomatic_complexity)
        
        # Nested for loops should have complexity 3 (1 base + 2 for loops)
        nested_for = next(f for f in result.function_list if f.name == 'ForLoopTest::nested_for')
        self.assertEqual(3, nested_for.cyclomatic_complexity)
        
        # Simple foreach should have complexity 2 (1 base + 1 loop)
        simple_foreach = next(f for f in result.function_list if f.name == 'ForLoopTest::simple_foreach_array')
        self.assertEqual(2, simple_foreach.cyclomatic_complexity)
        
        # Foreach with implicit variable should have complexity 2 (1 base + 1 loop)
        foreach_implicit = next(f for f in result.function_list if f.name == 'ForLoopTest::foreach_implicit')
        self.assertEqual(2, foreach_implicit.cyclomatic_complexity)
        
        # Foreach with hash should have complexity 2 (1 base + 1 loop)
        foreach_hash = next(f for f in result.function_list if f.name == 'ForLoopTest::foreach_hash')
        self.assertEqual(2, foreach_hash.cyclomatic_complexity)
        
        # For loop with control flow should have complexity 4 (1 base + 1 for + 2 conditions: next if, last if)
        for_control = next(f for f in result.function_list if f.name == 'ForLoopTest::for_with_control_flow')
        self.assertEqual(4, for_control.cyclomatic_complexity)

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