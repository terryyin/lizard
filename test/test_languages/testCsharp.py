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
