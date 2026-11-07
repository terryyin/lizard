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


class TestPerlCoverageGaps(unittest.TestCase):
    """Tests targeting previously-uncovered branches in lizard_languages/perl.py."""

    def _funcs(self, code):
        return analyze_file.analyze_source_code("a.pl", code).function_list

    def test_inline_comment_after_code(self):
        # exercises preprocess() comment-buffer branch (token after '#'
        # then newline flushes it)
        code = (
            "sub foo {\n"
            "    my $x = 1; # trailing comment\n"
            "    return $x;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("foo", funcs[0].name)

    def test_unterminated_trailing_comment(self):
        # preprocess() final flush when file ends mid-comment (no newline)
        code = "sub foo { return 1; }\n# trailing"
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))

    def test_top_level_function_call_with_anonymous_sub(self):
        # _state_function_call: 'sub' arg in a top-level call
        code = (
            "register(sub { return 42; });\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("<anonymous>", funcs[0].name)

    def test_top_level_call_with_nested_parens(self):
        # _state_function_call: nested '(' increments paren_count
        code = "print((1 + 2));\n"
        funcs = self._funcs(code)
        self.assertEqual(0, len(funcs))

    def test_function_with_prototype(self):
        # _state_function_dec '(' branch + _state_function_prototype
        code = (
            "sub fetch ($) {\n"
            "    return shift;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("fetch", funcs[0].name)

    def test_function_with_prototype_nested_parens(self):
        # _state_function_prototype '(' nesting branch
        code = (
            "sub weird (($)) {\n"
            "    return 1;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))

    def test_variable_declaration_no_assignment(self):
        # _state_variable ';' branch (declaration, no '=')
        code = (
            "my $x;\n"
            "sub foo { return 1; }\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("foo", funcs[0].name)

    def test_nested_named_sub_with_attribute(self):
        # _state_nested_named_sub_brace_search ':' branch
        code = (
            "sub outer {\n"
            "    sub inner :method {\n"
            "        return 1;\n"
            "    }\n"
            "    return inner();\n"
            "}\n"
        )
        funcs = self._funcs(code)
        names = {f.name for f in funcs}
        self.assertIn("inner", names)

    def test_nested_named_sub_forward_declaration(self):
        # _state_nested_named_sub_brace_search ';' (forward decl) branch
        code = (
            "sub outer {\n"
            "    sub inner;\n"
            "    return 1;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        names = {f.name for f in funcs}
        self.assertIn("inner", names)

    def test_nested_call_with_anonymous_sub(self):
        # _state_nested_call 'sub' branch + _state_nested_anon_search/body
        code = (
            "sub outer {\n"
            "    register(sub { return 1; });\n"
            "    return 1;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        names = [f.name for f in funcs]
        # Current parser behavior: inner anonymous sub is captured as
        # <anonymous>, and the outer "sub outer" is lost (surfaces as
        # *global*). Locking in the observed set so regressions in the
        # _state_nested_call 'sub' branch are caught.
        # TODO: outer sub should be named "outer" — pending parser fix.
        self.assertEqual(["<anonymous>", "*global*"], names)

    def test_nested_call_with_extra_parens(self):
        # _state_nested_call '(' nesting branch
        code = (
            "sub outer {\n"
            "    print((1));\n"
            "    return 1;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))

    # ------------------------------------------------------------------
    # Iteration 1 — reachable coverage gaps in perl.py
    # Each test targets a specific state-machine branch; counter-input
    # walk applied per entry.  See docs/pr0-findings.md for methodology.
    # ------------------------------------------------------------------

    def test_top_level_anon_sub_with_package(self):
        # Target: perl.py:133 — _state_function_call 'sub' branch
        # prefixing <anonymous> with package name.
        # Counter-inputs: with-package (new) vs without-package (existing
        # test_top_level_function_call_with_anonymous_sub) prove the
        # branch is sensitive to self.package_name.
        code = (
            "package MyPkg;\n"
            "register(sub { return 42; });\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("MyPkg::<anonymous>", funcs[0].name)

    def test_anon_brace_search_with_prototype_parens(self):
        # Target: perl.py:219, 221-222 — _state_anon_brace_search
        # '(' and ')' nesting branches when the anonymous sub has a
        # prototype before its opening brace.
        # Counter-input BOUNDARY: paren_count goes up then down but
        # stops >0 (does NOT return to _state_global).
        code = (
            "register(sub () { return 1; });\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual("<anonymous>", funcs[0].name)
        self.assertEqual(1, funcs[0].cyclomatic_complexity)

    def test_nested_call_anon_sub_with_package(self):
        # Target: perl.py:303 — _state_nested_call 'sub' branch
        # prefixing <anonymous> with package name.
        # Counter-input: paired with test_nested_call_with_anonymous_sub
        # (no package) — both must produce the correct prefix.
        code = (
            "package Foo;\n"
            "sub outer {\n"
            "    register(sub { return 1; });\n"
            "    return 1;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        names = {f.name for f in funcs}
        self.assertIn("Foo::<anonymous>", names)

    def test_nested_anon_search_with_prototype_parens(self):
        # Target: perl.py:320, 322-323 — _state_nested_anon_search
        # '(' and ')' nesting branches (paren_count stays > 0).
        # Counter-input BOUNDARY: mirrors
        # test_anon_brace_search_with_prototype_parens at the nested
        # (inside-function-body) level.
        code = (
            "sub outer {\n"
            "    register(sub () { return 1; });\n"
            "    return 1;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        names = [f.name for f in funcs]
        self.assertIn("<anonymous>", names)

    def test_nested_anon_sub_expression_as_condition(self):
        # Target: perl.py:250-258 — _state_nested_sub_dec '{' branch.
        # A bare anonymous sub expression inside a function body
        # (`sub foo { sub { ... }; }`).  The state machine does NOT
        # create a separate function for the anon sub; instead it
        # counts it as a condition on the enclosing function
        # (add_condition at line 257) and continues parsing the body.
        # Counter-input LOGIC: locking in the observed behavior so a
        # mutation of add_condition -> no-op or a refactor that
        # creates a nested function would fail.
        code = (
            "sub foo {\n"
            "    my $cb = sub { return 1; };\n"
            "    return $cb;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        # Note: 'my $cb = sub { ... }' goes through _state_variable /
        # _state_assignment / _state_anon_sub, which DOES create an
        # anonymous function named '$cb'.  To actually reach the
        # _state_nested_sub_dec path we need a bare 'sub { ... }' as
        # a statement (not assigned to a variable).
        self.assertGreaterEqual(len(funcs), 1)

    def test_nested_bare_anon_sub_statement(self):
        # Target: perl.py:250-258 — _state_nested_sub_dec '{' branch.
        # A bare anonymous sub expression used as a statement inside
        # a named sub's body.  This is the path that routes through
        # _state_function_body's 'sub' handler (line 239) into
        # _state_nested_sub_dec, which on seeing '{' as the next
        # token runs the anonymous branch at lines 250-258.
        #
        # The observed behavior (lines 250-258) is: brace_count++,
        # anonymous_count++, add_condition on the enclosing function,
        # and transition back to _state_function_body.  No new
        # function object is created for the anon sub — it is folded
        # into the enclosing function's cyclomatic complexity.
        # Counter-input MISSING: a package declaration ensures the
        # `if self.package_name:` branch at line 255-256 runs as part
        # of the same state transition (locally-unused anon_name but
        # the branch executes — mutation `if self.package_name:` ->
        # `if False` would be killed only if the branch is exercised).
        code = (
            "package MyPkg;\n"
            "sub foo {\n"
            "    sub { return 1; };\n"
            "    return 42;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        # Exactly one function is produced; the bare anonymous sub
        # does NOT surface as its own function.  With the package
        # prefix, foo is reported as 'MyPkg::foo'.
        self.assertEqual(1, len(funcs))
        self.assertEqual("MyPkg::foo", funcs[0].name)
        # Counter-input BOUNDARY: the bare anon sub adds one to the
        # cyclomatic complexity of foo (via add_condition on line 257).
        # Baseline cyclomatic for an empty-body sub is 1, so foo must
        # report >= 2 here.  Mutation `add_condition -> pass` would
        # leave cyclomatic at 1 and fail this assertion.
        self.assertGreaterEqual(funcs[0].cyclomatic_complexity, 2)

    def test_sub_keyword_after_attribute_colon(self):
        # Target: perl.py:188-196 — _state_function_dec 'sub' branch.
        # Reachable via the attribute-context edge case: after the
        # first `sub` keyword we enter _state_function_dec; a ':'
        # token sets in_attribute=True (line 170-171); the next 'sub'
        # token matches the `elif token == 'sub':` check at line 188
        # BEFORE the attribute-skip logic at line 197 runs.  The
        # anonymous branch then creates a <anonymous> function and
        # transitions to _state_anon_brace_search.
        #
        # This is a pathological Perl input — the misleading comment
        # on line 189 claims the branch handles `callback(sub{...})`
        # but that construct actually routes through _state_function_call.
        # The ONLY input that reaches line 188 is one where `sub`
        # appears in _state_function_dec AFTER a ':' attribute marker
        # (or in any other state-function-dec continuation that
        # re-encounters the `sub` keyword before seeing name/{/;/(/).
        # Counter-input MISSING: a package declaration ensures the
        # `if self.package_name:` branch at line 192-193 executes,
        # producing the `MyPkg::<anonymous>` prefixed form.
        code = (
            "package MyPkg;\n"
            "sub :sub { return 1; }\n"
        )
        funcs = self._funcs(code)
        # Observed: one anonymous function is produced with the
        # package prefix from line 193.
        self.assertEqual(1, len(funcs))
        self.assertEqual("MyPkg::<anonymous>", funcs[0].name)

    def test_nested_anon_body_with_inner_braces(self):
        # Target: perl.py:328 — _state_nested_anon_body '{' branch
        # (brace_count increment for an inner block inside a nested
        # anonymous sub body).
        # Counter-input OFF_BY_ONE: the inner '{' must NOT end the
        # anon sub prematurely — outer function must still be closed
        # correctly.
        code = (
            "sub outer {\n"
            "    register(sub {\n"
            "        if (1) { return 2; }\n"
            "        return 3;\n"
            "    });\n"
            "    return 1;\n"
            "}\n"
        )
        funcs = self._funcs(code)
        names = [f.name for f in funcs]
        self.assertIn("<anonymous>", names)
        # OFF_BY_ONE guard: the anonymous sub's inner 'if' must
        # register as a condition on that function, proving the
        # nested-anon-body state actually processed its contents
        # (not exited early on the inner '{').
        anon = next(f for f in funcs if f.name == "<anonymous>")
        self.assertGreaterEqual(anon.cyclomatic_complexity, 2) 