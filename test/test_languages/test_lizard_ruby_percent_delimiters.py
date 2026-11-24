import unittest
from lizard import analyze_file


class TestRubyPercentDelimiters(unittest.TestCase):
    def test_arbitrary_delimiters_keep_literals_opaque(self):
        for delimiter in ('@', '#', ';', '+', '-', '`', ':', '?', '='):
            for prefix in ('q', 'Q', 'w', 'W', 'i', 'I', 's', 'x', 'r'):
                with self.subTest(delimiter=delimiter, prefix=prefix):
                    literal = '%' + prefix + delimiter + ' if fake end ' + delimiter
                    source = ('def f; value = ' + literal +
                              '; if real then 1 end; end\ndef g; 1; end')
                    functions = analyze_file.analyze_source_code(
                        'sample.rb', source).function_list
                    self.assertEqual([('f', 2), ('g', 1)],
                                     [(f.name, f.cyclomatic_complexity)
                                      for f in functions])

    def test_escaped_delimiters_nesting_and_modulo(self):
        for expression in (r'%q@ literal \@ if fake end @',
                           '%q{ literal { if fake end } }', 'value % divisor'):
            with self.subTest(expression=expression):
                source = 'def f; x = ' + expression + '; if real then 1 end; end'
                function, = analyze_file.analyze_source_code(
                    'sample.rb', source).function_list
                self.assertEqual(2, function.cyclomatic_complexity)
