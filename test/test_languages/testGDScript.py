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
