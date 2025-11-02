import unittest

from lizard import FileAnalyzer, analyze_file, get_extensions


def get_solidity_fileinfo(source_code):
    return analyze_file.analyze_source_code('a.sol', source_code)


def get_solidity_function_list(source_code):
    return get_solidity_fileinfo(source_code).function_list


class TestSolidity(unittest.TestCase):
    def test_if(self):
        result = get_solidity_function_list(
            '''
            function foo(uint x) public pure returns (uint) {
                if (x < 10) {
                    return 0;
                } else {
                    return 2;
                }
            }
            '''
        )
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_function(self):
        result = get_solidity_function_list(
            '''
                function named()
                    public
                    pure
                    returns (
                        uint x,
                        bool b,
                        uint y
                    )
                {
                    return (1, true, 2);
                }
            '''
        )
        self.assertEqual(1, len(result))
        self.assertEqual('named', result[0].name)


class Test_Solidity_Cognitive_Complexity(unittest.TestCase):
    """Cognitive Complexity tests for Solidity"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        function simple(uint x) public pure returns (uint) {
            return x + 1;
        }
        '''
        functions = get_solidity_function_list(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        function check(uint x) public pure returns (string memory) {
            if (x > 0) {  // +1
                return "positive";
            }
            return "non-positive";
        }
        '''
        functions = get_solidity_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        function nested() public pure {
            for (uint i = 0; i < 10; i++) {           // +1
                for (uint j = 0; j < 10; j++) {       // +2 (nesting=1)
                    if (i == j) {                     // +3 (nesting=2)
                        // do something
                    }
                }
            }
        }
        // Total CogC = 6
        '''
        functions = get_solidity_function_list(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        function check(bool a, bool b, bool c, bool d, bool e) public pure returns (uint) {
            if (a && b && c) {  // +1 for if, +1 for && sequence
                return 1;
            }
            if (d || e) {       // +1 for if, +1 for || sequence
                return 2;
            }
            return 0;
        }
        // Total CogC = 4
        '''
        functions = get_solidity_function_list(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        function countdown(uint n) public pure returns (uint) {
            while (n > 0) {  // +1
                n = n - 1;
            }
            return n;
        }
        // Total CogC = 1
        '''
        functions = get_solidity_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
