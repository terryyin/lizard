import unittest

from lizard import whitelist_filter
from ..testHelpers import get_cpp_function_list


class TestCppOperatorDeclarations(unittest.TestCase):

    def test_using_literal_operator_does_not_swallow_next_function(self):
        functions = get_cpp_function_list('''
            using ::my::ns::operator""_fmt;
            enum class E { A, B, };
            int f(int a) { if (a) return 1; return 0; }
        ''')

        self.assertEqual(['f'], [function.name for function in functions])
        self.assertEqual(4, functions[0].start_line)
        self.assertEqual(2, functions[0].cyclomatic_complexity)

    def test_modulo_assignment_keeps_its_name_and_complexity(self):
        functions = get_cpp_function_list('''
            struct X {
                X& operator%=(int x) {
                    if (x) return *this;
                    return *this;
                }
            };
        ''')

        self.assertEqual(
            [('X::operator % =', 2)],
            [(function.name, function.cyclomatic_complexity)
             for function in functions])

    def test_literal_operator_keeps_its_existing_name(self):
        functions = get_cpp_function_list(
            'int operator""_km(unsigned long long x) { return x; }')

        self.assertEqual(
            ['operator "" _km'],
            [function.name for function in functions])

    def test_existing_operator_whitelist_still_matches(self):
        functions = get_cpp_function_list('''
            struct X {
                int operator+(int x) { if (x) return x; return 0; }
            };
        ''')

        warnings = list(whitelist_filter(functions, script='X::operator +'))

        self.assertEqual([], warnings)
