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


class Test_Lua_Cognitive_Complexity(unittest.TestCase):
    """Cognitive Complexity tests for Lua"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        function simple(x)
            return x + 1
        end
        '''
        functions = get_function_list(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        function check(x)
            if x > 0 then  -- +1
                return "positive"
            end
            return "non-positive"
        end
        '''
        functions = get_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        function nested()
            for i = 1, 10 do           -- +1
                for j = 1, 10 do       -- +2 (nesting=1)
                    if i == j then     -- +3 (nesting=2)
                        print(i)
                    end
                end
            end
        end
        -- Total CogC = 6
        '''
        functions = get_function_list(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        function check(a, b, c, d, e)
            if a and b and c then  -- +1 for if, +1 for 'and' sequence
                return 1
            end
            if d or e then         -- +1 for if, +1 for 'or' sequence
                return 2
            end
            return 0
        end
        -- Total CogC = 4
        '''
        functions = get_function_list(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_elseif_increments(self):
        """Each elseif counts as +1"""
        code = '''
        function classify(x)
            if x > 0 then      -- +1
                return "positive"
            elseif x < 0 then  -- +1
                return "negative"
            else               -- +1
                return "zero"
            end
        end
        -- Total CogC = 3
        '''
        functions = get_function_list(code)
        self.assertEqual(3, functions[0].cognitive_complexity)

    def test_repeat_until(self):
        """Lua's repeat-until should count as +1"""
        code = '''
        function countdown(n)
            repeat             -- +1
                n = n - 1
            until n == 0
            return n
        end
        -- Total CogC = 1
        '''
        functions = get_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        function count_down(n)
            while n > 0 do  -- +1
                n = n - 1
            end
            return n
        end
        -- Total CogC = 1
        '''
        functions = get_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
