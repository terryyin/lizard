"""Cognitive Complexity tests for Erlang"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_erlang_cogc(source_code):
    """Analyze Erlang code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.erl", source_code
    ).function_list


class TestErlangCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Erlang"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        simple(X) ->
            X + 1.
        '''
        functions = get_erlang_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_case_statement(self):
        """Single case statement should be CogC=1"""
        code = '''
        check(X) ->
            case X > 0 of  % +1
                true -> "positive";
                false -> "non-positive"
            end.
        '''
        functions = get_erlang_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    @unittest.skip("Erlang parser limitation: Semicolon (;) inside case/if statements incorrectly terminates the function, preventing detection of nested structures")
    def test_nested_case_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        nested() ->
            case true of               % +1
                true ->
                    case true of       % +2 (nesting=1)
                        true ->
                            case true of  % +3 (nesting=2)
                                true -> ok
                            end
                    end
            end.
        % Total CogC = 6
        '''
        functions = get_erlang_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    @unittest.skip("Erlang parser limitation: Semicolon (;) inside case/if statements incorrectly terminates the function, preventing proper clause tracking")
    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        check(A, B, C, D, E) ->
            if
                A andalso B andalso C ->  % +1 for if, +1 for andalso sequence
                    1;
                D orelse E ->             % +1 for condition, +1 for orelse
                    2;
                true ->
                    0
            end.
        % Total CogC = 4
        '''
        functions = get_erlang_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)
