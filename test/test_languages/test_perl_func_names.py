import unittest
from lizard import analyze_file
import os


class TestPerlFunctionNames(unittest.TestCase):

    def test_perl_function_names(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_func_name_issue.pl"))
        
        # We should have 4 functions
        self.assertEqual(4, len(result.function_list), 
                         f"Expected 4 functions but found {len(result.function_list)}: {[f.name for f in result.function_list]}")
        
        # Verify function names are correctly parsed
        function_names = sorted([f.name for f in result.function_list])
        
        # After fixing, all functions should be named 'fetch'
        expected_names = ['fetch', 'fetch', 'fetch', 'fetch'] 
        self.assertEqual(expected_names, function_names,
                        f"Expected {expected_names} but got {function_names}") 