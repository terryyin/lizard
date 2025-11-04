import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions

def get_gdscript_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.gd", source_code).function_list

class TestGDScript(unittest.TestCase):
    def test_empty(self):
        functions = get_gdscript_function_list("")
        self.assertEqual(0, len(functions))

    def test_top_level_function(self):
        functions = get_gdscript_function_list(
            "func a():\n" +
            "    pass")
        self.assertEqual(1, len(functions))

    def test_else_keyword_and_missing_elif(self):
        """
        BUG 1: GDScript _conditions included 'else' keyword (now removed in refactoring)
        BUG 2: GDScript is missing 'elif' keyword in _control_flow_keywords
        
        GDScript uses Python-like syntax with if/elif/else, but elif wasn't included.
        This test documents the current behavior.
        """
        code = '''
func test_if_else(x, y):
    if x > 0:
        print("positive")
    else:
        print("not positive")
    
    if y > 10:
        return "high"
    elif y > 5:
        return "medium"
    else:
        return "low"
        '''
        functions = get_gdscript_function_list(code)
        self.assertEqual(1, len(functions))
        
        # Fixed: 4 = base(1) + if(1) + if(1) + elif(1)
        current_ccn = functions[0].cyclomatic_complexity
        
        # Bug fixed: elif now counted correctly
        self.assertEqual(4, current_ccn, 
                        "GDScript should count elif keyword")
