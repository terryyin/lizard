import unittest
from lizard import analyze_file
from lizard_languages.r import RReader


class TestNestedRParameters(unittest.TestCase):
    def test_backtick_quoted_identifiers_are_single_tokens(self):
        self.assertEqual(
            ['`x, ) y`', ' ', '=', ' ', 'list', '(', '1', ',', ' ', '2', ')'],
            list(RReader.generate_tokens('`x, ) y` = list(1, 2)')),
        )
        self.assertEqual(
            [r'`a\`b`', ' ', '=', ' ', '1'],
            list(RReader.generate_tokens(r'`a\`b` = 1')),
        )

    def test_nested_defaults_preserve_formal_parameter_count(self):
        samples = [
            ('a=list(1, 2, 3), b=3', ['a', 'b']),
            ('a=list(1), b=3, c=4', ['a', 'b', 'c']),
            ('a=matrix(1, nrow=2, ncol=3), b=4', ['a', 'b']),
            ('a=values[1, 2], b=values[[1]], c=3', ['a', 'b', 'c']),
            ('a={list(1, 2)}, b=3', ['a', 'b']),
            ('', []),
        ]
        for parameters, names in samples:
            with self.subTest(parameters=parameters):
                source = ('f <- function(' + parameters + ') { if (ok) { 1 } }'
                          '\ng <- function(z) { z }')
                functions = analyze_file.analyze_source_code(
                    'sample.r', source).function_list
                self.assertEqual(['f', 'g'], [f.name for f in functions])
                self.assertEqual(names, functions[0].parameters)
                self.assertEqual(len(names), functions[0].parameter_count)
                self.assertEqual(2, functions[0].cyclomatic_complexity)
                self.assertEqual(2, functions[1].start_line)
                self.assertEqual(['z'], functions[1].parameters)

    def test_ordinary_and_empty_formals_render_each_token_once(self):
        ordinary = analyze_file.analyze_source_code(
            'sample.r',
            'f <- function(x, y = 2) { if (ok) { 1 } }\ng <- function(z) { z }',
        ).function_list
        self.assertEqual(['x', 'y'], ordinary[0].parameters)
        self.assertEqual(2, ordinary[0].parameter_count)
        self.assertEqual(['x', 'y = 2'], ordinary[0].full_parameters)
        self.assertEqual('f( x , y = 2)', ordinary[0].long_name)
        self.assertEqual(2, ordinary[0].cyclomatic_complexity)
        self.assertEqual(2, ordinary[1].start_line)
        self.assertEqual(['z'], ordinary[1].parameters)

        empty = analyze_file.analyze_source_code(
            'sample.r',
            'f <- function() { if (ok) { 1 } }\ng <- function(z) { z }',
        ).function_list
        self.assertEqual([], empty[0].parameters)
        self.assertEqual(0, empty[0].parameter_count)
        self.assertEqual([], empty[0].full_parameters)
        self.assertEqual('f()', empty[0].long_name)
        self.assertEqual(2, empty[0].cyclomatic_complexity)
        self.assertEqual(2, empty[1].start_line)

    def test_variadic_and_quoted_formals_with_nested_defaults(self):
        samples = [
            (
                'a = list(1, 2), b = 3',
                ['a', 'b'],
                ['a = list ( 1 , 2 )', 'b = 3'],
                'f( a = list ( 1 , 2 ) , b = 3)',
            ),
            (
                'a = list(1, 2), ...',
                ['a', '...'],
                ['a = list ( 1 , 2 )', '...'],
                'f( a = list ( 1 , 2 ) , ...)',
            ),
            (
                '`x y` = list(1, 2), b',
                ['x y', 'b'],
                ['`x y` = list ( 1 , 2 )', 'b'],
                'f( `x y` = list ( 1 , 2 ) , b)',
            ),
            (
                'a = {list(1, 2)[1]}, ..., `x, ) y` = list(1, 2)',
                ['a', '...', 'x, ) y'],
                ['a = { list ( 1 , 2 ) [ 1 ] }', '...', '`x, ) y` = list ( 1 , 2 )'],
                'f( a = { list ( 1 , 2 ) [ 1 ] } , ... , `x, ) y` = list ( 1 , 2 ))',
            ),
            (
                'na.rm = TRUE, ...',
                ['na.rm', '...'],
                ['na . rm = TRUE', '...'],
                'f( na . rm = TRUE , ...)',
            ),
        ]
        for parameters, names, full_params, long_name in samples:
            with self.subTest(parameters=parameters):
                source = ('f <- function(' + parameters + ') { if (ok) { 1 } }'
                          '\ng <- function(z) { z }')
                functions = analyze_file.analyze_source_code(
                    'sample.r', source).function_list
                self.assertEqual(['f', 'g'], [f.name for f in functions])
                self.assertEqual(names, functions[0].parameters)
                self.assertEqual(len(names), functions[0].parameter_count)
                self.assertEqual(full_params, functions[0].full_parameters)
                self.assertEqual(long_name, functions[0].long_name)
                self.assertEqual(2, functions[0].cyclomatic_complexity)
                self.assertEqual(2, functions[1].start_line)
                self.assertEqual(['z'], functions[1].parameters)
