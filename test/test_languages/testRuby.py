import unittest

from lizard import analyze_file


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


class Test_parser_for_Ruby_ccn(unittest.TestCase):

    def test_escaped_interpolation_markers_do_not_add_decisions(self):
        code = r'''
def account_template
  "Account: \#{user.name if user}"
end

def rendered_template(user, active)
  "Account: \#{user.name if user}; viewer: #{user.name if active}"
end

def path_label(user)
  "labels\\#{user.name if user}"
end

def path_template
  "labels\\\#{user.name if user}"
end
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive']))

        functions = analyzer.analyze_source_code('labels.rb', code).function_list

        self.assertEqual(
            [('account_template', 1, 0), ('rendered_template', 2, 1),
             ('path_label', 2, 1), ('path_template', 1, 0)],
            [(fn.name, fn.cyclomatic_complexity, fn.cognitive_complexity)
             for fn in functions])

    def test_basic_complexity(self):
        result = get_ruby_function_list('''
            def f
                if a
                elsif b
                end
            end
                ''')
        self.assertEqual(3, result[0].cyclomatic_complexity)


class Test_parser_for_Ruby_def(unittest.TestCase):
    def test_class_method(self):
        result = get_ruby_function_list('''
            def a.b
            end
                ''')
        self.assertEqual("a.b", result[0].name)
        self.assertEqual(0, result[0].parameter_count)

    def test_empty_parameters(self):
        result = get_ruby_function_list('''
            def a()
            end
                ''')
        self.assertEqual(0, result[0].parameter_count)

    def test_more_parameters(self):
        result = get_ruby_function_list('''
            def a(b,c)
            end
                ''')
        self.assertEqual(2, result[0].parameter_count)

    def test_rails_controller_with_percent_i_and_safe_navigation(self):
        """Issue #457: Parser must not hang on %i[] and &. constructs"""
        code = '''
before_action :adapt_widget_params, only: %i[add check_duplicate update_type_fields]
def adapt_widget_params
  @account_group_id = params[:presenter]&.delete(:account_group_id)
  @widget_params = params[:presenter] || {}
end
def adapt_search_params
  @account_group_id = params[:search]&.delete(:account_group_id)
end
'''
        result = get_ruby_function_list(code)
        self.assertEqual(2, len(result))
        self.assertEqual("adapt_widget_params", result[0].name)
        self.assertEqual("adapt_search_params", result[1].name)


class Test_Ruby_comment_line_continuation(unittest.TestCase):
    """A '#' comment ending in a backslash must not continue onto the next
    line and swallow it (issue #317, the shared-tokenizer half)."""

    def test_backslash_at_end_of_comment_keeps_next_line(self):
        code = (
            "def foo(x)\n"
            "  # a trailing comment\\\n"
            "  if x > 10\n"
            "    return 1\n"
            "  end\n"
            "  return 0\n"
            "end\n"
        )
        result = get_ruby_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)
