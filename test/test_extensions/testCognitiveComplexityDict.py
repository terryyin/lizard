"""API presence of cognitive_complexity on FunctionInfo.__dict__."""
import unittest

from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcognitive import LizardExtension as Cognitive


def analyze(code):
    return FileAnalyzer(get_extensions([Cognitive()])).analyze_source_code(
        "a.cpp", code)


class TestCognitiveComplexityDict(unittest.TestCase):

    def test_zero_score_is_present_in_function_dict(self):
        # CLI CogC 0 must still expose the field for Python API consumers.
        result = analyze("void g() {} void h() { if (a) x(); }")
        scores = [f.cognitive_complexity for f in result.function_list]
        self.assertEqual([0, 1], scores)
        for function, score in zip(result.function_list, scores):
            self.assertIn(
                'cognitive_complexity', function.__dict__,
                msg="%s should expose cognitive_complexity in __dict__"
                % function.name)
            self.assertEqual(score, function.__dict__['cognitive_complexity'])

    def test_field_absent_without_the_extension(self):
        result = FileAnalyzer(get_extensions([])).analyze_source_code(
            "a.cpp", "void g() {}")
        self.assertNotIn(
            'cognitive_complexity', result.function_list[0].__dict__)
