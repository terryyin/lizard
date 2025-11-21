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

    def test_expression_bodied_methods(self):
        """Test that expression-bodied methods (lambda-style syntax) are detected."""
        result = get_csharp_function_list('''
            using System;

            public class SimpleCalculator
            {
                public int Add(int a, int b) => a + b;

                public int Subtract(int a, int b)
                {
                    int result = a - b;
                    return result;
                }

                public static void Main(string[] args)
                {
                    var calc = new SimpleCalculator();
                    Console.WriteLine($"Addition: {calc.Add(30, 15)}");
                    Console.WriteLine($"Subtraction: {calc.Subtract(30, 15)}");
                }
            }
        ''')
        
        function_names = [f.name for f in result]
        self.assertIn("SimpleCalculator::Add", function_names,
                     f"Expected to find 'SimpleCalculator::Add' but only found: {function_names}")
        self.assertIn("SimpleCalculator::Subtract", function_names,
                     f"Expected to find 'SimpleCalculator::Subtract' but only found: {function_names}")
        self.assertIn("SimpleCalculator::Main", function_names,
                     f"Expected to find 'SimpleCalculator::Main' but only found: {function_names}")
        
        self.assertEqual(3, len(result),
                        f"Expected 3 functions but found {len(result)}: {function_names}")
        
        add_func = next(f for f in result if f.name == "SimpleCalculator::Add")
        self.assertEqual(1, add_func.cyclomatic_complexity,
                        "Expression-bodied Add method should have complexity of 1")

    def test_expression_bodied_methods(self):
        """Test that expression-bodied methods (lambda-style syntax) are detected."""
        result = get_csharp_function_list('''
            using System;

            public class SimpleCalculator
            {
                public int Add(int a, int b) => a + b;

                public int Subtract(int a, int b)
                {
                    int result = a - b;
                    return result;
                }

                public static void Main(string[] args)
                {
                    var calc = new SimpleCalculator();
                    Console.WriteLine($"Addition: {calc.Add(30, 15)}");
                    Console.WriteLine($"Subtraction: {calc.Subtract(30, 15)}");
                }
            }
        ''')
        
        function_names = [f.name for f in result]
        self.assertIn("SimpleCalculator::Add", function_names,
                     f"Expected to find 'SimpleCalculator::Add' but only found: {function_names}")
        self.assertIn("SimpleCalculator::Subtract", function_names,
                     f"Expected to find 'SimpleCalculator::Subtract' but only found: {function_names}")
        self.assertIn("SimpleCalculator::Main", function_names,
                     f"Expected to find 'SimpleCalculator::Main' but only found: {function_names}")
        
        self.assertEqual(3, len(result),
                        f"Expected 3 functions but found {len(result)}: {function_names}")
        
        add_func = next(f for f in result if f.name == "SimpleCalculator::Add")
        self.assertEqual(1, add_func.cyclomatic_complexity,
                        "Expression-bodied Add method should have complexity of 1")
