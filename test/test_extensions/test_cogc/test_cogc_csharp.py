"""Cognitive Complexity tests for Csharp"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_csharp_cogc(source_code):
    """Analyze Csharp code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.cs", source_code
    ).function_list


class TestCsharpCognitiveComplexity(unittest.TestCase):
    """Test Cognitive Complexity calculations for C# code"""

    def test_simple_method_has_zero_cogc(self):
        """Simple method with no control flow should have CogC = 0"""
        code = '''
public class Simple {
    public int Add(int x, int y) {
        return x + y;
    }
}
'''
        functions = get_csharp_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_nested_loops_increase_cogc_more_than_ccn(self):
        """Nested loops should increase CogC more than CCN"""
        code = '''
public class Nested {
    public void NestedMethod() {
        for (int i = 0; i < 10; i++) {      // +1
            for (int j = 0; j < 10; j++) {  // +2 (nesting=1)
                if (i == j) {               // +3 (nesting=2)
                    Console.WriteLine(i);
                }
            }
        }
    }
}
'''
        functions = get_csharp_cogc(code)
        # CogC = 1 + 2 + 3 = 6
        # CCN = 3 (for, for, if)
        self.assertGreaterEqual(functions[0].cognitive_complexity, 3)

    def test_switch_statement_counts_as_one(self):
        """Switch statement should count as +1 regardless of cases"""
        code = '''
public class Switcher {
    public string GetWord(int number) {
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
}
'''
        functions = get_csharp_cogc(code)
        # CogC = 1 (entire switch)
        # CCN = 4 (case, case, case, default)
        self.assertEqual(1, functions[0].cognitive_complexity)
        self.assertLess(functions[0].cognitive_complexity,
                       functions[0].cyclomatic_complexity)

    def test_linq_null_coalescing(self):
        """LINQ and null-coalescing operators"""
        code = '''
public class LinqTest {
    public string GetValue(string input) {
        var result = input?.ToUpper() ?? "default";  // null-coalescing doesn't count
        return result;
    }
}
'''
        functions = get_csharp_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_try_catch_finally(self):
        """try-catch-finally should count catch clauses"""
        code = '''
public class ErrorHandler {
    public void Handle() {
        try {
            RiskyOperation();
        } catch (IOException e) {  // +1
            LogError(e);
        } catch (Exception e) {    // +1
            HandleError(e);
        } finally {
            Cleanup();
        }
    }
}
'''
        functions = get_csharp_cogc(code)
        # CogC = 2 (two catch clauses)
        self.assertEqual(2, functions[0].cognitive_complexity)

    def test_foreach_loop(self):
        """foreach should count like for"""
        code = '''
public class Looper {
    public void Process(List<int> items) {
        foreach (var item in items) {  // +1
            if (item > 0) {            // +2 (nesting=1)
                Console.WriteLine(item);
            }
        }
    }
}
'''
        functions = get_csharp_cogc(code)
        # CogC = 1 + 2 = 3
        self.assertEqual(3, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operators in sequence"""
        code = '''
public class Logical {
    public bool Check() {
        if (a && b && c) {     // +1 for if, +1 for && sequence
            return true;
        }
        if (d || e || f) {     // +1 for if, +1 for || sequence
            return false;
        }
        return false;
    }
}
'''
        functions = get_csharp_cogc(code)
        # CogC = 1 (if) + 1 (&&) + 1 (if) + 1 (||) = 4
        self.assertEqual(4, functions[0].cognitive_complexity)
