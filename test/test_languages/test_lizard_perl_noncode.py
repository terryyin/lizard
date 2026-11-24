import unittest
from lizard import analyze_file


class TestPerlNonCodeSections(unittest.TestCase):
    def test_pod_does_not_define_functions(self):
        for directive in ('=pod', '=head1 EXAMPLES', '=over 4'):
            with self.subTest(directive=directive):
                code = directive + '\nsub fake { if ($x) {} }\n=cut\nsub real { return 1; }\n'
                result = analyze_file.analyze_source_code('sample.pl', code)
                self.assertEqual(['real'], [f.name for f in result.function_list])
                self.assertEqual(1, result.nloc)
                self.assertEqual(4, result.function_list[0].start_line)

    def test_data_markers_end_executable_source(self):
        for marker in ('__DATA__', '__END__'):
            with self.subTest(marker=marker):
                code = 'sub real { return 1; }\n' + marker + '\nsub fake { if ($x) {} }\n'
                result = analyze_file.analyze_source_code('sample.pl', code)
                self.assertEqual(['real'], [f.name for f in result.function_list])
                self.assertEqual(1, result.nloc)

    def test_unterminated_pod_is_documentation_to_eof(self):
        code = 'sub real { 1; }\n=pod\nsub fake { if ($x) {} }\n'
        result = analyze_file.analyze_source_code('sample.pl', code)
        self.assertEqual(['real'], [f.name for f in result.function_list])

    def test_data_marker_prefixes_are_normal_names(self):
        code = 'sub __DATA__value { 1; }\nsub __END__value { 2; }\n'
        result = analyze_file.analyze_source_code('sample.pl', code)
        self.assertEqual(['__DATA__value', '__END__value'],
                         [f.name for f in result.function_list])
