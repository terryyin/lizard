"""Cognitive Complexity tests for C++"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_cpp_cogc(source_code):
    """Analyze C++ code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.cpp", source_code
    ).function_list


class TestCppCognitiveComplexity(unittest.TestCase):
    """Test Cognitive Complexity calculations for C/C++ code"""

    def test_simple_function_has_zero_cogc(self):
        """Simple function with no control flow should have CogC = 0"""
        code = '''
int add(int x, int y) {
    return x + y;
}
'''
        functions = get_cpp_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_labeled_continue_with_nested_loops(self):
        """Test labeled continue with nested loops - CogC should be 7"""
        code = '''
int processValues(int limit) {
    int result = 0;
    OUTER: for (int i = 1; i <= limit; ++i) {  // +1
        for (int j = 2; j < i; ++j) {          // +2 (nesting=1)
            if (i % j == 0) {                  // +3 (nesting=2)
                continue OUTER;                // +1 (labeled jump)
            }
        }
        result += i;
    }
    return result;
}
'''
        functions = get_cpp_cogc(code)
        # FROM COGNITIVE COMPLEXITY SPEC: CogC = 7 (1+2+3+1)
        self.assertEqual(7, functions[0].cognitive_complexity)
        self.assertEqual(4, functions[0].cyclomatic_complexity)

    def test_switch_statement_counts_as_one(self):
        """Switch statement should count as +1 regardless of cases"""
        code = '''
const char* getWord(int number) {
    switch (number) {  // +1 for entire switch
        case 1:
            return "one";
        case 2:
            return "a couple";
        case 3:
            return "a few";
        default:
            return "lots";
    }
}
'''
        functions = get_cpp_cogc(code)
        # CogC = 1 (entire switch)
        # CCN = 4 (case, case, case, default)
        self.assertEqual(1, functions[0].cognitive_complexity)
        self.assertEqual(4, functions[0].cyclomatic_complexity)

    def test_else_if_no_nesting_penalty(self):
        """else if should not get nesting penalty"""
        code = '''
const char* classify(int x) {
    if (x > 10) {        // +1
        return "big";
    } else if (x > 5) {  // +1 (hybrid - no nesting)
        return "medium";
    } else if (x > 0) {  // +1 (hybrid - no nesting)
        return "small";
    } else {             // +1 (hybrid - no nesting)
        return "zero or negative";
    }
}
'''
        functions = get_cpp_cogc(code)
        # CogC = 4 (all at nesting level 0)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_ternary_operator(self):
        """Ternary operator counts as +1"""
        code = '''
int absolute(int x) {
    return x > 0 ? x : -x;  // +1 for ternary
}
'''
        functions = get_cpp_cogc(code)
        # SPEC REQUIREMENT: Ternary operator adds +1 per spec
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_try_catch_cpp(self):
        """C++ try-catch should count catch clauses"""
        code = '''
void errorHandler() {
    try {
        riskyOperation();
    } catch (std::runtime_error& e) {  // +1
        logError(e);
    } catch (...) {                     // +1
        handleError();
    }
}
'''
        functions = get_cpp_cogc(code)
        # CogC = 2 (two catch clauses)
        self.assertEqual(2, functions[0].cognitive_complexity)

    def test_while_loop_with_nested_if(self):
        """While loop with nested if shows nesting penalty"""
        code = '''
void whileNested() {
    while (condition) {     // +1
        if (check) {        // +2 (nesting=1)
            break;
        }
    }
}
'''
        functions = get_cpp_cogc(code)
        # CogC = 1 + 2 = 3
        # CCN = 2
        self.assertEqual(3, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operators in sequence"""
        code = '''
bool logical() {
    if (a && b && c) {     // +1 for if, +1 for && sequence
        return true;
    }
    if (d || e || f) {     // +1 for if, +1 for || sequence
        return false;
    }
    return false;
}
'''
        functions = get_cpp_cogc(code)
        # CogC = 1 (if) + 1 (&&) + 1 (if) + 1 (||) = 4
        self.assertEqual(4, functions[0].cognitive_complexity)
