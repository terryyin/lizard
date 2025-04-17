import unittest
from lizard import analyze_file
from lizard_languages.python import PythonReader

def get_python_function_list(source_code):
    return analyze_file.analyze_source_code("test.py", source_code).function_list

class TestPythonForgive(unittest.TestCase):
    
    def test_python_forgive_above_function(self):
        code = '''
# #lizard forgive
def complex_function():
    x = 1
    if x > 0:
        print("Positive")
    elif x < 0:
        print("Negative")
    else:
        print("Zero")
    return x
'''
        functions = get_python_function_list(code)
        self.assertEqual(0, len(functions), "Function with forgive directive above should be forgiven")
    
    def test_python_forgive_inside_function(self):
        code = '''
def complex_function():
    # #lizard forgive
    x = 1
    if x > 0:
        print("Positive")
    elif x < 0:
        print("Negative")
    else:
        print("Zero")
    return x
'''
        functions = get_python_function_list(code)
        self.assertEqual(0, len(functions), "Function with forgive directive inside should be forgiven")
    
    def test_python_forgive_with_various_formats(self):
        # Testing different comment formats
        formats = [
            "# #lizard forgive",
            "#lizard forgive",
            "##lizard forgive",
            "# lizard forgive"
        ]
        
        for comment in formats:
            code = f'''
def complex_function():
    {comment}
    x = 1
    if x > 0:
        print("Positive")
    return x
'''
            functions = get_python_function_list(code)
            self.assertEqual(0, len(functions), f"Function with forgive directive '{comment}' should be forgiven")

if __name__ == "__main__":
    unittest.main() 