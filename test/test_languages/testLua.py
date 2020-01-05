import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import LuaReader


def get_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.lua", source_code).function_list

class Test_tokenizing_Lua(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(LuaReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_should_not_confuse_division_as_regx(self):
        self.check_tokens(['a','/','b',',','a','/','b'], 'a/b,a/b')
        self.check_tokens(['3453',' ','/','b',',','a','/','b'], '3453 /b,a/b')

    def test_double_square_brackets_string(self):
        self.check_tokens(['[[this is a string]]'], '[[this is a string]]')
        self.check_tokens(['[==[this is a string]==]'], '[==[this is a string]==]')

    def test_comment(self):
        self.check_tokens(['a', ' ', '--this is a comment', '\n'], "a --this is a comment\n")

    def test_comment_block(self):
        comment = '''--[[ multiple
        line
        comment]]'''
        self.check_tokens([comment, "\n"], comment + "\n")



class Test_parser_for_Lua(unittest.TestCase):

    def test_empty(self):
        functions = get_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_function_list(''' p "1" ''')
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_function_list('''
            function f
            end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("f", result[0].name)
        self.assertEqual(0, result[0].parameter_count)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(2, result[0].length)
        self.assertEqual(2, result[0].token_count)


    def test_comment_is_omitted_in_token_cound(self):
        result = get_function_list('''
            function f
            -- this is a comment
            end
                ''')
        self.assertEqual(2, result[0].token_count)


    def test_one_function_loc(self):
        result = get_function_list('''
            function f
                something
            end
                ''')
        self.assertEqual(3, result[0].length)
        self.assertEqual(3, result[0].nloc)

    def test_two_functions(self):
        result = get_function_list('''
            function f
            end
            function g
            end
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual("g", result[1].name)

    def test_do(self):
        result = get_function_list('''
            function k
            end
            do
                function g
                end
            end
            function f
            end
                ''')
        self.assertEqual(3, len(result))

    def test_for_and_while(self):
        result = get_function_list('''
        function factorial(n)
          local x = 1
          for i = 2, n do
            x = x * i
          end
          while a do
            a=a-1
          end
          return x
        end
        function g
        end
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_repeat_until(self):
        result = get_function_list('''
        function f(n)
            repeat
              --statements
            until condition
        end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_if(self):
        result = get_function_list('''
        function a(n)
            if a then
            elseif b then
            else
            end
        end
        function a(n)
        end
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_class_method(self):
        result = get_function_list('''
        function V.f(n)
        end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual('V.f', result[0].name)

    def test_anonymous(self):
        result = get_function_list('''
            function(self, neigh, id)
            end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual('(anonymous)', result[0].name)

    def test_anonymous_with_assignment(self):
        result = get_function_list('''
            a = function(self, neigh, id)
            end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual('a', result[0].name)

    def test_nested_functions(self):
        result = get_function_list('''
        function addn(x)
          function sum(y)
            return x+y
          end
          return sum
        end
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual('sum', result[0].name)
        self.assertEqual('addn', result[1].name)
