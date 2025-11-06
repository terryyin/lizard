"""Cognitive Complexity tests for Lua"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_lua_cogc(source_code):
    """Analyze Lua code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.lua", source_code
    ).function_list


class TestLuaCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Lua"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        function simple(x)
            return x + 1
        end
        '''
        functions = get_lua_cogc(code)
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
        functions = get_lua_cogc(code)
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
        functions = get_lua_cogc(code)
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
        functions = get_lua_cogc(code)
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
        functions = get_lua_cogc(code)
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
        functions = get_lua_cogc(code)
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
        functions = get_lua_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
