import unittest
from lizard import analyze_file


class TestLanguageRegression(unittest.TestCase):
    def test_valid_source(self):
        samples = [('def real\n 1\nend\n__END__\ndef fake\n if x\n end\nend\n',
          [('real', 1)]),
         ('def real\n=begin\nif fake\n=end\n1\nend\n', [('real', 1)]),
         ('=begin example\n'
          'def fake\n'
          ' if x\n'
          ' end\n'
          'end\n'
          '=end\n'
          'def real\n'
          '1\n'
          'end\n',
          [('real', 1)])]
        for source, expected in samples:
            with self.subTest(source=source):
                result = analyze_file.analyze_source_code(
                    "sample.rb", source)
                self.assertEqual(expected, [(f.name, f.cyclomatic_complexity)
                                            for f in result.function_list])
                self.assertEqual(3, result.nloc)

    def test_data_marker_prefix_is_still_code(self):
        source = 'def real\n__END__value\n if x\n  1\n end\nend\n'
        result = analyze_file.analyze_source_code('sample.rb', source)
        self.assertEqual(6, result.nloc)
        self.assertEqual([('real', 2)],
                         [(f.name, f.cyclomatic_complexity)
                          for f in result.function_list])
