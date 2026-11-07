import unittest
from lizard import analyze_file


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


class Test_Ruby_regex_literal_context(unittest.TestCase):

    def test_return_regex_exposes_ternary_condition(self):
        functions = get_ruby_function_list(
            'def a(s)\n return /#/ =~ s ? 1 : 0\nend\n')
        self.assertEqual([('a', 2)], [
            (f.name, f.cyclomatic_complexity) for f in functions])

    def test_member_division_keeps_condition_and_tokens(self):
        functions = get_ruby_function_list(
            'def divide(obj,a,b,c)\n obj.return / (a || b) / c\nend\n')
        self.assertEqual(1, len(functions))
        self.assertEqual(2, functions[0].cyclomatic_complexity)
        self.assertEqual(22, functions[0].token_count)

    def test_split_and_gsub_regex_controls_keep_conditions(self):
        split_fn = get_ruby_function_list(
            'def a(s)\n s.split(/"/).include?(s) || s.nil?\nend\n')[0]
        gsub_fn = get_ruby_function_list(
            'def a(s)\n s.gsub(/#/, "") || s\nend\n')[0]
        self.assertEqual(2, split_fn.cyclomatic_complexity)
        self.assertEqual(2, gsub_fn.cyclomatic_complexity)
