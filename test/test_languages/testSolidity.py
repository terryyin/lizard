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


