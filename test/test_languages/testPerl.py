import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from ..testHelpers import get_cpp_function_list_with_extension
import os
from lizard_languages.code_reader import CodeReader


class TestPerl(unittest.TestCase):

    def test_perl_function(self):
        code = '''#!/usr/bin/perl

use strict;
use warnings;

# A simple Perl subroutine with complexity
sub sub_with_complexity {
    my ($param1, $param2) = @_;
    
    if ($param1 > 0) {
        print "Positive\n";
    }
    
    return $param1 + $param2;
}'''
        result = analyze_file.analyze_source_code("test.pl", code)
        self.assertEqual(1, len(result.function_list))
        self.assertEqual('sub_with_complexity', result.function_list[0].name)
        self.assertEqual(2, result.function_list[0].cyclomatic_complexity)

    def test_perl_package_methods(self):
        code = '''#!/usr/bin/perl

use strict;
use warnings;

package MyPackage;

# A method in a package
sub my_method {
    my ($self, $param1) = @_;
    
    if ($param1 > 0) {
        print "Positive\n";
    }
    elsif ($param1 < 0) {
        print "Negative\n";
    }
    
    return $param1 * 2;
}

# Another method to test multiple methods in package
sub another_method {
    my ($self) = @_;
    return 42;
}

1; # End of package'''
        result = analyze_file.analyze_source_code("test.pl", code)
        self.assertEqual(2, len(result.function_list))
        
        # Test first method
        method1 = next(f for f in result.function_list if f.name == 'MyPackage::my_method')
        self.assertEqual(3, method1.cyclomatic_complexity)  # 1 + 2 conditions (if, elsif)
        
        # Test second method
        method2 = next(f for f in result.function_list if f.name == 'MyPackage::another_method')
        self.assertEqual(1, method2.cyclomatic_complexity)

    def test_perl_method_attributes(self):
        code = '''#!/usr/bin/perl

use strict;
use warnings;

package TestAttributes;

# Method with a single attribute
sub method_with_attr : method {
    my ($self) = @_;
    return "I'm a method";
}

# Method with multiple attributes
sub lvalue_method : lvalue : method {
    my ($self) = @_;
    my $value = 42;
    $value;  # returns lvalue
}

# Method with attribute and conditions
sub complex_method : locked {
    my ($self, $x) = @_;
    
    if ($x > 0) {
        return "positive";
    }
    elsif ($x < 0) {
        return "negative";
    }
    else {
        return "zero";
    }
}

1; # End of package'''
        result = analyze_file.analyze_source_code("test.pl", code)
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
        
        # One-liner with condition should have higher complexity
        condition = next(f for f in result.function_list if f.name == 'OneLinerTest::condition_oneliner')
        self.assertEqual(5, condition.cyclomatic_complexity)  # 1 base + 2 for ? + 2 for :
        
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

    def test_perl_compound_conditions(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_compound_conditions.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(8, len(result.function_list), f"Found functions: {function_names}")
        
        # Simple AND has complexity 3 (1 base + 1 if + 1 && operator)
        simple_and = next(f for f in result.function_list if f.name == 'CompoundConditionTest::simple_and')
        self.assertEqual(3, simple_and.cyclomatic_complexity)
        
        # Simple OR has complexity 3 (1 base + 1 if + 1 || operator)
        simple_or = next(f for f in result.function_list if f.name == 'CompoundConditionTest::simple_or')
        self.assertEqual(3, simple_or.cyclomatic_complexity)
        
        # Multiple AND has complexity 4 (1 base + 1 if + 2 && operators)
        multiple_and = next(f for f in result.function_list if f.name == 'CompoundConditionTest::multiple_and')
        self.assertEqual(4, multiple_and.cyclomatic_complexity)
        
        # Multiple OR has complexity 4 (1 base + 1 if + 2 || operators)
        multiple_or = next(f for f in result.function_list if f.name == 'CompoundConditionTest::multiple_or')
        self.assertEqual(4, multiple_or.cyclomatic_complexity)
        
        # Mixed conditions has complexity 4 (1 base + 1 if + 1 && + 1 ||)
        mixed = next(f for f in result.function_list if f.name == 'CompoundConditionTest::mixed_conditions')
        self.assertEqual(4, mixed.cyclomatic_complexity)
        
        # Complex conditions with parentheses has complexity 5 (1 base + 1 if + 1 && + 2 ||)
        complex_parens = next(f for f in result.function_list if f.name == 'CompoundConditionTest::complex_with_parens')
        self.assertEqual(5, complex_parens.cyclomatic_complexity)
        
        # Compound in loop has complexity 3 (1 base + 1 while + 1 &&)
        compound_loop = next(f for f in result.function_list if f.name == 'CompoundConditionTest::compound_in_loop')
        self.assertEqual(3, compound_loop.cyclomatic_complexity)
        
        # Compound with assignment has complexity 3 (1 base + 1 while + 1 &&)
        compound_assign = next(f for f in result.function_list if f.name == 'CompoundConditionTest::compound_with_assignment')
        self.assertEqual(3, compound_assign.cyclomatic_complexity)

    def test_perl_ternary_operator(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_ternary.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(7, len(result.function_list), f"Found functions: {function_names}")
        
        # Simple ternary has complexity 5 (1 base + 2 ? + 2 :)
        simple = next(f for f in result.function_list if f.name == 'TernaryTest::simple_ternary')
        self.assertEqual(5, simple.cyclomatic_complexity)
        
        # Multiple ternary has complexity 9 (1 base + 4 ? + 4 :)
        multiple = next(f for f in result.function_list if f.name == 'TernaryTest::multiple_ternary')
        self.assertEqual(9, multiple.cyclomatic_complexity)
        
        # Complex ternary has complexity 11 (1 base + 4 ? + 4 : + 2 && operators)
        complex_ternary = next(f for f in result.function_list if f.name == 'TernaryTest::complex_ternary')
        self.assertEqual(11, complex_ternary.cyclomatic_complexity)
        
        # Ternary in assignment has complexity 7
        assignment = next(f for f in result.function_list if f.name == 'TernaryTest::ternary_in_assignment')
        self.assertEqual(7, assignment.cyclomatic_complexity)
        
        # Ternary in return has complexity 5
        ret = next(f for f in result.function_list if f.name == 'TernaryTest::ternary_in_return')
        self.assertEqual(5, ret.cyclomatic_complexity)
        
        # Ternary in function call has complexity 5
        call = next(f for f in result.function_list if f.name == 'TernaryTest::ternary_in_call')
        self.assertEqual(5, call.cyclomatic_complexity)
        
        # Nested ternary has complexity 9
        nested = next(f for f in result.function_list if f.name == 'TernaryTest::nested_ternary')
        self.assertEqual(9, nested.cyclomatic_complexity)

    def test_perl_nested_subroutines(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_nested_subs.pl"))
        
        function_names = [f.name for f in result.function_list]
        # At minimum these functions should be detected
        expected_functions = {
            'NestedSubTest::nested_sub',
            'NestedSubTest::multi_nested', 
            'NestedSubTest::outer_with_complex_nested',
            'NestedSubTest::with_lexical_sub',
            'NestedSubTest::with_anon_in_block'
        }
        
        # Check that all expected functions are found
        for func_name in expected_functions:
            self.assertIn(func_name, function_names, f"Function {func_name} not found in {function_names}")
        
        # Test nested sub
        nested = next(f for f in result.function_list if f.name == 'NestedSubTest::nested_sub')
        self.assertEqual(1, nested.cyclomatic_complexity)
        
        # Function with lexical sub
        lexical = next(f for f in result.function_list if f.name == 'NestedSubTest::with_lexical_sub')
        self.assertEqual(1, lexical.cyclomatic_complexity)
        
        # Anonymous sub in a block
        anon_block = next(f for f in result.function_list if f.name == 'NestedSubTest::with_anon_in_block')
        self.assertEqual(2, anon_block.cyclomatic_complexity)  # 1 base + 1 if statement

    def test_perl_block_scoping(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_block_scoping.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(5, len(result.function_list), f"Found functions: {function_names}")
        
        # Simple block scope should have complexity 1
        simple_block = next(f for f in result.function_list if f.name == 'BlockScopingTest::with_block_scope')
        self.assertEqual(1, simple_block.cyclomatic_complexity)
        
        # Multiple nested blocks should have complexity 1
        multi_block = next(f for f in result.function_list if f.name == 'BlockScopingTest::multi_block_scope')
        self.assertEqual(1, multi_block.cyclomatic_complexity)
        
        # Block with conditional should have complexity 2 (1 base + 1 if)
        condition_block = next(f for f in result.function_list if f.name == 'BlockScopingTest::block_with_condition')
        self.assertEqual(2, condition_block.cyclomatic_complexity)
        
        # Block with loop should have complexity 2 (1 base + 1 while)
        loop_block = next(f for f in result.function_list if f.name == 'BlockScopingTest::block_with_loop')
        self.assertEqual(2, loop_block.cyclomatic_complexity)
        
        # Block with for loop should have complexity 2 (1 base + 1 for)
        for_block = next(f for f in result.function_list if f.name == 'BlockScopingTest::block_with_for')
        self.assertEqual(2, for_block.cyclomatic_complexity)

    def test_perl_package_scoping(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_package_scope.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(7, len(result.function_list), f"Found functions: {function_names}")
        
        # Functions from FirstPackage should be properly named
        first_pkg_funcs = [f for f in result.function_list if f.name.startswith('FirstPackage::')]
        self.assertEqual(4, len(first_pkg_funcs))
        
        # Functions from SecondPackage should be properly named
        second_pkg_funcs = [f for f in result.function_list if f.name.startswith('SecondPackage::')]
        self.assertEqual(3, len(second_pkg_funcs))
        
        # Function in first package
        first_access = next(f for f in result.function_list if f.name == 'FirstPackage::access_package_var')
        self.assertEqual(1, first_access.cyclomatic_complexity)
        
        # Another function in first package
        modify = next(f for f in result.function_list if f.name == 'FirstPackage::modify_package_var')
        self.assertEqual(1, modify.cyclomatic_complexity)
        
        # Function in second package with same name but different namespace
        second_access = next(f for f in result.function_list if f.name == 'SecondPackage::access_package_var')
        self.assertEqual(1, second_access.cyclomatic_complexity)
        
        # Complex function that accesses variables from both packages
        complex_func = next(f for f in result.function_list if f.name == 'SecondPackage::complex_package_access')
        self.assertEqual(2, complex_func.cyclomatic_complexity)  # 1 base + 1 if
        
        # Function defined after package switch
        another = next(f for f in result.function_list if f.name == 'FirstPackage::another_function')
        self.assertEqual(1, another.cyclomatic_complexity)

    def test_perl_given_when(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_given_when.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(5, len(result.function_list), f"Found functions: {function_names}")
        
        # Simple given-when should have complexity 6 (1 base + 1 given + 3 when + 1 default)
        simple = next(f for f in result.function_list if f.name == 'GivenWhenTest::simple_given_when')
        self.assertEqual(6, simple.cyclomatic_complexity)
        
        # Given-when with smart matching should have complexity 6 (1 base + 1 given + 3 when + 1 default)
        smart_match = next(f for f in result.function_list if f.name == 'GivenWhenTest::given_when_smart_match')
        self.assertEqual(6, smart_match.cyclomatic_complexity)
        
        # Given-when with nested logic should have complexity 10 (1 base + 1 given + 2 when + 1 default + 1 nested given + 2 nested when + 1 nested default + 1 if)
        nested = next(f for f in result.function_list if f.name == 'GivenWhenTest::given_when_with_nested')
        self.assertEqual(10, nested.cyclomatic_complexity)
        
        # Given-when with continue should have complexity 6 (1 base + 1 given + 3 when + 1 default)
        continue_case = next(f for f in result.function_list if f.name == 'GivenWhenTest::given_when_with_continue')
        self.assertEqual(6, continue_case.cyclomatic_complexity)
        
        # Given-when with complex conditions should have complexity 8 (1 base + 1 given + 5 when + 1 default)
        complex_case = next(f for f in result.function_list if f.name == 'GivenWhenTest::given_when_complex')
        self.assertEqual(8, complex_case.cyclomatic_complexity)

    def test_perl_do_while(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_do_while.pl"))
        
        function_names = [f.name for f in result.function_list]
        self.assertEqual(5, len(result.function_list), f"Found functions: {function_names}")
        
        # Simple do-while should have complexity 3 (1 base + 1 do + 1 while condition)
        simple = next(f for f in result.function_list if f.name == 'DoWhileTest::simple_do_while')
        self.assertEqual(3, simple.cyclomatic_complexity)
        
        # Do-while with complex condition should have complexity 4 (1 base + 1 do + 2 operators: > and &&)
        complex_do = next(f for f in result.function_list if f.name == 'DoWhileTest::complex_do_while')
        self.assertEqual(4, complex_do.cyclomatic_complexity)
        
        # Nested do-while loops should have complexity 5 (1 base + 2 do + 2 while conditions)
        nested = next(f for f in result.function_list if f.name == 'DoWhileTest::nested_do_while')
        self.assertEqual(5, nested.cyclomatic_complexity)
        
        # Do-while with if inside should have complexity 4 (1 base + 1 do + 1 while + 1 if)
        with_if = next(f for f in result.function_list if f.name == 'DoWhileTest::do_while_with_if')
        self.assertEqual(4, with_if.cyclomatic_complexity)
        
        # Do-while with control flow should have complexity 5 (1 base + 1 do + 1 while + 2 conditions: next if, last if)
        control = next(f for f in result.function_list if f.name == 'DoWhileTest::do_while_with_control')
        self.assertEqual(5, control.cyclomatic_complexity)

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
                self.assertEqual(0, len(result.function_list))
            finally:
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

    def test_perl_forgive_global(self):
        code = '''
# Global code with complexity
my $x = 1;
if ($x > 0) {
    print "Positive\\n";
}
elsif ($x < 0) {
    print "Negative\\n";
}
else {
    print "Zero\\n";
}

# #lizard forgive global
# More global code with complexity
my $y = 2;
if ($y > 0) {
    print "Y is positive\\n";
}
elsif ($y < 0) {
    print "Y is negative\\n";
}

# This function should still be counted
sub test_function {
    my ($param) = @_;
    if ($param > 0) {
        print "Param is positive\\n";
    }
    elsif ($param < 0) {
        print "Param is negative\\n";
    }
    else {
        print "Param is zero\\n";
    }
}
'''
        result = analyze_file.analyze_source_code("test.pl", code)
        
        # Should have one function (test_function) since global code is forgiven
        self.assertEqual(1, len(result.function_list))
        
        # Verify the function is the one we expect
        function = result.function_list[0]
        self.assertEqual("test_function", function.name)
        self.assertEqual(3, function.cyclomatic_complexity)  # 1 base + 2 conditions (else doesn't count) 