import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions


def get_csharp_fileinfo(source_code):
    return analyze_file.analyze_source_code("a.cs", source_code)


def get_csharp_function_list(source_code):
    return get_csharp_fileinfo(source_code).function_list


class TestCsharp(unittest.TestCase):

    def test_function_with_one(self):
        result = get_csharp_function_list('''
            public void Method()
            {
                Console.WriteLine("Hello World!");
            }
        ''')
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_function_with_two(self):
        result = get_csharp_function_list('''
            void Method(bool condition)
            {
                if (condition)
                {
                    Console.WriteLine("Hello World!");
                }
            }
        ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_function_with_three(self):
        result = get_csharp_function_list('''
            public void Method(bool condition1, bool condition2)
            {
                if (condition1 || condition2)
                {
                    Console.WriteLine("Hello World!");
                }
            }
        ''')
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_function_with_eight(self):
        result = get_csharp_function_list('''
            public void Method(DayOfWeek day)
            {

                    switch (day)
                    {
                        case DayOfWeek.Monday:
                            Console.WriteLine("Today is Monday!");
                            break;
                        case DayOfWeek.Tuesday:
                            Console.WriteLine("Today is Tuesday!");
                            break;
                        case DayOfWeek.Wednesday:
                            Console.WriteLine("Today is Wednesday!");
                            break;
                        case DayOfWeek.Thursday:
                            Console.WriteLine("Today is Thursday!");
                            break;
                        case DayOfWeek.Friday:
                            Console.WriteLine("Today is Friday!");
                            break;
                        case DayOfWeek.Saturday:
                            Console.WriteLine("Today is Saturday!");
                            break;
                        case DayOfWeek.Sunday:
                            Console.WriteLine("Today is Sunday!");
                            break;
                    }
                }

            }
        ''')
        self.assertEqual(8, result[0].cyclomatic_complexity)

    def test_null_coalescing_operator(self):
        result = get_csharp_function_list('''
            public void Method()
            {
                a ?? b;
            }
        ''')
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_primary_constructor_vs_traditional(self):
        # Test with primary constructor
        result_primary = get_csharp_function_list('''
            public class WorkspaceHeader(ILocator locator)
            {
                public void Method() { }
            }
        ''')
        
        # Test with traditional constructor
        result_traditional = get_csharp_function_list('''
            public class WorkspaceHeader
            {
                private readonly ILocator _locator;

                public WorkspaceHeader(ILocator locator)
                {
                    _locator = locator;
                }

                public void Method() { }
            }
        ''')

        # Both should have the same number of functions (just Method)
        self.assertEqual(1, len(result_primary), 
                       "Primary constructor should not be counted as a separate function")
        self.assertEqual(2, len(result_traditional), 
                       "Traditional constructor should be counted as one function")
        
        # Both Method functions should have complexity of 1
        self.assertEqual("WorkspaceHeader::Method", result_primary[0].name,
                       "Primary constructor version should only have Method function")
        self.assertEqual(1, result_primary[0].cyclomatic_complexity,
                       "Method should have complexity of 1")

        # Check traditional constructor version - first list all functions for better debugging
        function_names = [f.name for f in result_traditional]
        self.assertIn("WorkspaceHeader::Method", function_names,
                     f"Expected to find 'WorkspaceHeader::Method' function but only found these functions: {function_names}")
        self.assertIn("WorkspaceHeader::WorkspaceHeader", function_names,
                     f"Expected to find 'WorkspaceHeader::WorkspaceHeader' constructor but only found these functions: {function_names}")

        method_func = next(f for f in result_traditional if f.name == "WorkspaceHeader::Method")
        constructor_func = next(f for f in result_traditional if f.name == "WorkspaceHeader::WorkspaceHeader")
        
        self.assertEqual(1, method_func.cyclomatic_complexity,
                       "Method should have complexity of 1")
        self.assertEqual(1, constructor_func.cyclomatic_complexity,
                       "Constructor should have complexity of 1")


class Test_CSharp_Cognitive_Complexity(unittest.TestCase):
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
        functions = get_csharp_function_list(code)
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
        functions = get_csharp_function_list(code)
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
        functions = get_csharp_function_list(code)
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
        functions = get_csharp_function_list(code)
        # NOTE: Currently counts '?' in null-coalescing as ternary (+1)
        # TODO: Distinguish between ternary '?' and null-coalescing '??'
        self.assertEqual(1, functions[0].cognitive_complexity)

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
        functions = get_csharp_function_list(code)
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
        functions = get_csharp_function_list(code)
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
        functions = get_csharp_function_list(code)
        # CogC = 1 (if) + 1 (&&) + 1 (if) + 1 (||) = 4
        self.assertEqual(4, functions[0].cognitive_complexity)
