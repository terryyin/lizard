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

    def test_interpolated_string_with_quoted_brace(self):
        result = get_csharp_function_list(r'''
            class Labels
            {
                public string Join(string[] values)
                {
                    return $"{string.Join("\n{", values)}";
                }

                public int Count() { return 1; }
            }
        ''')
        self.assertEqual(
            [("Labels::Join", 4, 7), ("Labels::Count", 9, 9)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_interpolated_format_with_slashes(self):
        result = get_csharp_function_list(r'''
            class Labels
            {
                public string Format(System.DateTime date)
                {
                    return $"{date:yyyy//MM}";
                }

                public string Count(string label = "count") { return label; }
            }
        ''')
        self.assertEqual(
            [("Labels::Format", 4, 7), ("Labels::Count", 9, 9)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_interpolated_format_after_block_comment(self):
        result = get_csharp_function_list(r'''
            class Labels
            {
                public string Format(int count)
                {
                    return $"{count /* total */:0\\'}";
                }

                public string Footer() { return "*/}"; }
            }
        ''')
        self.assertEqual(
            [("Labels::Format", 4, 7), ("Labels::Footer", 9, 9)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_nullable_nested_type_argument_is_no_condition(self):
        result = get_csharp_function_list('''
            public void Method()
            {
                Dictionary<string, List<int>?> groups = new Dictionary<string, List<int>?>();
            }
        ''')
        self.assertEqual(1, result[0].cyclomatic_complexity)

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

    def test_verbatim_identifier_is_not_a_keyword(self):
        result = get_csharp_function_list('''
            class Parser
            {
                int Weight(Token token) { return token.@if + token.@event; }
                int Sum(int @case, int @while) { return @case + @while; }
            }
        ''')
        self.assertEqual([1, 1], [f.cyclomatic_complexity for f in result])

    def test_verbatim_identifier_is_not_a_structure(self):
        result = FileAnalyzer(get_extensions(['nd', 'ns', 'cognitive'])).analyze_source_code("a.cs", '''
            class Parser
            {
                int Sum(int @if, int @while) { return @if + @while; }
            }
        ''').function_list
        self.assertEqual(0, result[0].max_nesting_depth)
        self.assertEqual(0, result[0].max_nested_structures)
        self.assertEqual(0, result[0].cognitive_complexity)

    def test_verbatim_identifier_is_not_a_switch_in_modified_ccn(self):
        result = FileAnalyzer(get_extensions(['modified'])).analyze_source_code("a.cs", '''
            class Router
            {
                int Pick(int @switch) { return @switch; }
            }
        ''').function_list
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_method_with_a_verbatim_name(self):
        result = get_csharp_function_list('''
            class Parser
            {
                int @if() { return 1; }
                int Check() { return @if(); }
            }
        ''')
        self.assertEqual(["Parser::if", "Parser::Check"], [f.name for f in result])

    def test_class_with_a_verbatim_name(self):
        result = get_csharp_function_list('''
            class Node {}
            class @class : Node
            {
                void Visit() {}
            }
        ''')
        self.assertEqual(["class::Visit"], [f.name for f in result])

    def test_verbatim_name_in_a_recursive_call(self):
        result = FileAnalyzer(get_extensions(['cognitive'])).analyze_source_code("a.cs", '''
            class Walker
            {
                void Walk() { @Walk(); }
            }
        ''').function_list
        self.assertEqual(1, result[0].cognitive_complexity)

    def test_interpolation_counts_code_and_excludes_format_text(self):
        source = """
class Labels
{
    static string Render(bool ready, bool active, int count)
    {
        return $"{{if}} {((ready && active) ? "{" : "}")} {count:00//00}";
    }
    static int Next() { return 1; }
}
"""
        analyzer = FileAnalyzer(get_extensions(['nd', 'cognitive']))
        functions = analyzer.analyze_source_code("labels.cs", source).function_list
        self.assertEqual(['Labels::Render', 'Labels::Next'], [f.name for f in functions])
        render = functions[0]
        self.assertEqual((3, 2, 2), (
            render.cyclomatic_complexity, render.max_nesting_depth, render.cognitive_complexity))

    def test_multiline_interpolation_does_not_count_blank_or_comment_lines(self):
        source = """
class Labels
{
    static string Render(int count)
    {
        return $"{(
            // Render the count when it is available.

            count.ToString()
        )}";
    }
}
"""
        render, = get_csharp_function_list(source)
        self.assertEqual((4, 11, 6), (render.start_line, render.end_line, render.nloc))

    def test_csharp_line_endings_end_comments_inside_and_outside_interpolation(self):
        source = '''class Labels
{
    static string Render(bool ready)
    {
        return $"{( // Choose a label.<newline>ready ? "{" : "}")}";
    }
    // Keep the following method visible.<newline>static int Next() { return 1; }
}'''
        for ending in ('\n', '\r\n', '\r', '\x85', '\u2028', '\u2029'):
            with self.subTest(ending=repr(ending)):
                functions = get_csharp_function_list(source.replace('<newline>', ending))
                self.assertEqual([
                    ('Labels::Render', 3, 7, 2, 5),
                    ('Labels::Next', 9, 9, 1, 1),
                ], [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity, f.nloc)
                    for f in functions])

    def test_nested_interpolation_keeps_following_method(self):
        source = """
using System;
using System.Linq;
class Labels
{
    static string Join(string[] values) {
        return $"{string.Join(";", values.Select(value => $"{value.Replace("x", "{")}"))}";
    }
    static int Next() { return 1; }
}
"""
        functions = get_csharp_function_list(source)
        self.assertEqual([('Labels::Join', 6, 8), ('Labels::Next', 9, 9)], [
            (f.name, f.start_line, f.end_line) for f in functions])

    def test_interpolation_in_property_does_not_declare_a_function(self):
        source = """
class Job
{
    string Label => $"{nameof(Job)}";
    int Read() => 1;
}
"""
        functions = get_csharp_function_list(source)
        self.assertEqual(['Job::Read'], [f.name for f in functions])

    def test_raw_and_verbatim_string_contents_do_not_leak_into_metrics(self):
        source = r'''
class Labels
{
    static string Render(bool ready)
    {
        var path = @"C:\";
        var text = """"var a = """; if (ignored) {"""";
        return ready ? path : text;
    }
    static int Next() { return 1; }
}
'''
        functions = get_csharp_function_list(source)
        self.assertEqual([('Labels::Render', 2), ('Labels::Next', 1)], [
            (f.name, f.cyclomatic_complexity) for f in functions])

    def test_raw_and_verbatim_interpolations_count_only_holes(self):
        source = r'''
class Labels
{
    static string Render(bool ready)
    {
        var raw = $$"""if (ignored) { {{(ready ? "{" : "}")}} }""";
        var path = @$"C:\{(ready ? "{" : "}")}\";
        return raw + path;
    }
    static int Next() { return 1; }
}
'''
        functions = get_csharp_function_list(source)
        self.assertEqual([('Labels::Render', 3), ('Labels::Next', 1)], [
            (f.name, f.cyclomatic_complexity) for f in functions])

    def test_nullable_and_null_conditional_marks_are_not_ternaries(self):
        source = """
#nullable enable
class Labels
{
    static string Render(string[]? values, bool ready)
    {
        string? label = values?[0];
        int? length = label?.Length;
        return (ready ? label?.Trim() : null) ?? $"{default(int?)} {length?.ToString()}";
    }
}
"""
        analyzer = FileAnalyzer(get_extensions(['nd', 'cognitive', 'complextags']))
        render, = analyzer.analyze_source_code("labels.cs", source).function_list
        self.assertEqual((3, 1, 1), (
            render.cyclomatic_complexity, render.max_nesting_depth, render.cognitive_complexity))
        self.assertEqual([['?', 9], ['??', 9]], render.complex_tags)

    def test_ternary_inside_assertion_remains_visible_to_extensions(self):
        source = """
class Labels
{
    static void Check(bool enabled)
    {
        assert(enabled ? default(int?) == null : false);
    }
    static void assert(bool condition) {}
}
"""
        analyzer = FileAnalyzer(get_extensions(['ignoreassert', 'complextags']))
        check = analyzer.analyze_source_code("labels.cs", source).function_list[0]
        self.assertEqual(1, check.cyclomatic_complexity)
        self.assertEqual([['?', 6]], check.complex_tags)

    def test_collection_expression_is_a_ternary_arm(self):
        source = """
class Labels
{
    static int[] Build(bool ready, int[]? previous)
    {
        int[]? entries = ready ? [1] : [2];
        return ready ? [previous?[0] ?? 0] : entries;
    }
}
"""
        analyzer = FileAnalyzer(get_extensions(['complextags']))
        build, = analyzer.analyze_source_code("labels.cs", source).function_list
        self.assertEqual(4, build.cyclomatic_complexity)
        self.assertEqual([['?', 6], ['?', 7], ['??', 7]], build.complex_tags)

    def test_nullable_generic_local_return_type_is_not_a_condition(self):
        source = """
#nullable enable
using System.Collections.Generic;
class Labels
{
    static int Count()
    {
        List<T>? Get<T>() where T : class { return null; }
        return Get<string>()?.Count ?? 0;
    }
}
"""
        count, = get_csharp_function_list(source)
        self.assertEqual(2, count.cyclomatic_complexity)
