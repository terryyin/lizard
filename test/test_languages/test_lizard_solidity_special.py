import unittest
from lizard import analyze_file


class TestSoliditySpecialFunctions(unittest.TestCase):
    def test_constructor_receive_and_fallback(self):
        code = ("contract C {\n"
                " constructor(uint x) { if (x > 0) {} }\n"
                " receive() external payable { if (true) {} }\n"
                " fallback() external { if (false) {} }\n"
                " function ordinary() public {}\n}\n")
        functions = analyze_file.analyze_source_code('sample.sol', code).function_list
        self.assertEqual(['constructor', 'receive', 'fallback', 'ordinary'],
                         [f.name for f in functions])
        self.assertEqual([2, 2, 2, 1], [f.cyclomatic_complexity for f in functions])
        self.assertEqual([2, 3, 4, 5], [f.start_line for f in functions])
