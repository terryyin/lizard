"""Tests for the Halstead complexity extension (``-Ehalstead``)."""

import unittest
from collections import Counter

from .testHelpers import (
    get_python_function_list_with_extension,
    get_cpp_function_list_with_extension,
)
from lizard_ext.lizardhalstead import (
    LizardExtension as HalsteadExtension,
    HalsteadMetrics,
    HalsteadClassifier,
    PythonHalsteadClassifier,
    get_classifier,
)


def get_python_function(source):
    return get_python_function_list_with_extension(
        source, HalsteadExtension())[0]


def get_cpp_function(source):
    return get_cpp_function_list_with_extension(
        source, HalsteadExtension())[0]


#: A small Python function whose Halstead numbers are verified by hand and
#: reused across several tests.
SAMPLE = '''def foo(a, b):
    c = a + b * 2
    if c > 10 and a != b:
        return c
    return a - b
'''


class TestHalsteadBasicCounts(unittest.TestCase):

    def setUp(self):
        self.function = get_python_function(SAMPLE)

    def test_distinct_operators_n1(self):
        # ( , ) : = + * if > and != return -
        self.assertEqual(13, self.function.halstead.distinct_operators)

    def test_distinct_operands_n2(self):
        # a b c 2 10
        self.assertEqual(5, self.function.halstead.distinct_operands)

    def test_total_operators_N1(self):
        self.assertEqual(15, self.function.halstead.total_operators)

    def test_total_operands_N2(self):
        self.assertEqual(13, self.function.halstead.total_operands)

    def test_operand_occurrences(self):
        operands = self.function._halstead_operands
        self.assertEqual(4, operands['a'])
        self.assertEqual(4, operands['b'])
        self.assertEqual(3, operands['c'])

    def test_paired_delimiters_counted_separately(self):
        operators = self.function._halstead_operators
        self.assertEqual(1, operators['('])
        self.assertEqual(1, operators[')'])


class TestHalsteadDerivedMeasures(unittest.TestCase):

    def setUp(self):
        self.halstead = get_python_function(SAMPLE).halstead

    def test_vocabulary(self):
        self.assertEqual(18, self.halstead.vocabulary)

    def test_length(self):
        self.assertEqual(28, self.halstead.length)

    def test_volume(self):
        self.assertAlmostEqual(116.7579, self.halstead.volume, places=3)

    def test_difficulty(self):
        self.assertAlmostEqual(16.9, self.halstead.difficulty, places=3)

    def test_effort(self):
        self.assertAlmostEqual(1973.2085, self.halstead.effort, places=3)

    def test_time(self):
        self.assertAlmostEqual(109.6227, self.halstead.time, places=3)

    def test_bugs(self):
        self.assertAlmostEqual(0.038919, self.halstead.bugs, places=5)


class TestHalsteadFunctionInfoAttributes(unittest.TestCase):
    """The flat, rounded attributes used for display/sort/threshold."""

    def setUp(self):
        self.function = get_python_function(SAMPLE)

    def test_volume_attribute_is_rounded(self):
        self.assertEqual(116.76, self.function.halstead_volume)

    def test_difficulty_attribute_is_rounded(self):
        self.assertEqual(16.9, self.function.halstead_difficulty)

    def test_effort_attribute_is_rounded(self):
        self.assertEqual(1973.21, self.function.halstead_effort)

    def test_basic_count_attributes(self):
        self.assertEqual(13, self.function.halstead_n1)
        self.assertEqual(5, self.function.halstead_n2)
        self.assertEqual(15, self.function.halstead_N1)
        self.assertEqual(13, self.function.halstead_N2)

    def test_attributes_default_to_zero_without_extension(self):
        # A function that never went through the extension still answers the
        # Halstead attributes (with empty counts) rather than raising.
        from lizard import FunctionInfo
        fresh = FunctionInfo('bar', 'a.py', 0)
        self.assertEqual(0, fresh.halstead_n1)
        self.assertEqual(0.0, fresh.halstead_volume)
        self.assertEqual(0.0, fresh.halstead.difficulty)


class TestHalsteadClassification(unittest.TestCase):

    def test_keywords_acting_as_operators(self):
        function = get_python_function(SAMPLE)
        operators = function._halstead_operators
        self.assertEqual(1, operators['if'])
        self.assertEqual(1, operators['and'])
        self.assertEqual(2, operators['return'])

    def test_function_name_belongs_to_enclosing_scope(self):
        # By convention the def keyword and the function name are attributed to
        # the enclosing scope, exactly like token_count, so they do not appear
        # among the function's own operands/operators.
        function = get_python_function(SAMPLE)
        self.assertNotIn('foo', function._halstead_operands)
        self.assertNotIn('def', function._halstead_operators)

    def test_string_literal_is_operand(self):
        function = get_python_function(
            'def f():\n    return "hello"\n')
        self.assertIn('"hello"', function._halstead_operands)

    def test_numeric_literal_is_operand(self):
        function = get_python_function('def f():\n    return 42\n')
        self.assertIn('42', function._halstead_operands)

    def test_python_value_literals_are_operands(self):
        function = get_python_function(
            'def f(x):\n'
            '    if x:\n'
            '        return True\n'
            '    return None\n')
        self.assertIn('True', function._halstead_operands)
        self.assertIn('None', function._halstead_operands)
        self.assertNotIn('True', function._halstead_operators)
        self.assertNotIn('None', function._halstead_operators)


class TestHalsteadClassifierUnit(unittest.TestCase):
    """Directly exercise the classifier objects."""

    def setUp(self):
        self.python = PythonHalsteadClassifier()
        self.generic = HalsteadClassifier()

    def test_python_keyword_operators(self):
        for token in ('if', 'else', 'for', 'return', 'and', 'or', 'not',
                      'def', 'class', 'import', 'lambda', 'yield'):
            self.assertEqual(HalsteadClassifier.OPERATOR,
                             self.python.classify(token), token)

    def test_python_value_literals_are_operands(self):
        for token in ('True', 'False', 'None'):
            self.assertEqual(HalsteadClassifier.OPERAND,
                             self.python.classify(token), token)

    def test_python_soft_keywords_are_operands(self):
        # match/case/type/_ are context-dependent; treat them as identifiers.
        for token in ('match', 'case', 'type', '_'):
            self.assertEqual(HalsteadClassifier.OPERAND,
                             self.python.classify(token), token)

    def test_identifiers_are_operands(self):
        for token in ('foo', 'bar_baz', '_private', 'CamelCase'):
            self.assertEqual(HalsteadClassifier.OPERAND,
                             self.python.classify(token), token)

    def test_symbols_are_operators(self):
        for token in ('+', '-', '*', '==', '!=', '(', ')', '{', '}',
                      ':', ',', '.', '&&'):
            self.assertEqual(HalsteadClassifier.OPERATOR,
                             self.python.classify(token), token)

    def test_string_and_number_tokens(self):
        self.assertEqual(HalsteadClassifier.OPERAND,
                         self.python.classify('"a string"'))
        self.assertEqual(HalsteadClassifier.OPERAND,
                         self.python.classify("'c'"))
        self.assertEqual(HalsteadClassifier.OPERAND,
                         self.python.classify('0xFF'))
        self.assertEqual(HalsteadClassifier.OPERAND,
                         self.python.classify('42'))

    def test_leading_dot_float_is_operand(self):
        # Some tokenizers (e.g. C-family) emit ``.5`` as a single token.
        for token in ('.5', '.5e3', '.25E-4', '0.5', '5.'):
            self.assertEqual(HalsteadClassifier.OPERAND,
                             self.generic.classify(token), token)

    def test_bare_dot_and_ellipsis_stay_operators(self):
        for token in ('.', '..', '...', '.foo'):
            self.assertEqual(HalsteadClassifier.OPERATOR,
                             self.generic.classify(token), token)

    def test_generic_c_family_keywords(self):
        self.assertEqual(HalsteadClassifier.OPERATOR,
                         self.generic.classify('int'))
        self.assertEqual(HalsteadClassifier.OPERATOR,
                         self.generic.classify('if'))
        self.assertEqual(HalsteadClassifier.OPERAND,
                         self.generic.classify('true'))
        self.assertEqual(HalsteadClassifier.OPERAND,
                         self.generic.classify('null'))
        self.assertEqual(HalsteadClassifier.OPERAND,
                         self.generic.classify('myvar'))

    def test_empty_token_is_skipped(self):
        self.assertIs(HalsteadClassifier.SKIP, self.python.classify(''))


class TestClassifierSelection(unittest.TestCase):

    class _Reader(object):
        def __init__(self, **attrs):
            self.__dict__.update(attrs)

    def test_python_reader_gets_python_classifier(self):
        reader = self._Reader(language_names=['python'])
        self.assertIsInstance(get_classifier(reader), PythonHalsteadClassifier)

    def test_unknown_language_gets_generic_classifier(self):
        reader = self._Reader(language_names=['cpp'])
        classifier = get_classifier(reader)
        self.assertIsInstance(classifier, HalsteadClassifier)
        self.assertNotIsInstance(classifier, PythonHalsteadClassifier)

    def test_reader_without_language_names_gets_generic(self):
        self.assertIsInstance(get_classifier(self._Reader()),
                              HalsteadClassifier)

    def test_reader_hook_takes_precedence(self):
        sentinel = PythonHalsteadClassifier()
        reader = self._Reader(language_names=['cpp'],
                              halstead_classifier=sentinel)
        self.assertIs(sentinel, get_classifier(reader))


class TestHalsteadMetricsObject(unittest.TestCase):
    """The value object computes the formulae from raw multisets."""

    def test_empty_metrics_are_zero(self):
        metrics = HalsteadMetrics()
        self.assertEqual(0, metrics.vocabulary)
        self.assertEqual(0, metrics.length)
        self.assertEqual(0.0, metrics.volume)
        self.assertEqual(0.0, metrics.difficulty)
        self.assertEqual(0.0, metrics.effort)
        self.assertEqual(0.0, metrics.calculated_length)

    def test_formulae(self):
        operators = Counter({'+': 3, '=': 2})        # n1=2, N1=5
        operands = Counter({'a': 4, 'b': 1})         # n2=2, N2=5
        metrics = HalsteadMetrics(operators, operands)
        self.assertEqual(2, metrics.distinct_operators)
        self.assertEqual(2, metrics.distinct_operands)
        self.assertEqual(5, metrics.total_operators)
        self.assertEqual(5, metrics.total_operands)
        self.assertEqual(4, metrics.vocabulary)
        self.assertEqual(10, metrics.length)
        # V = 10 * log2(4) = 20
        self.assertAlmostEqual(20.0, metrics.volume, places=6)
        # D = (2 / 2) * (5 / 2) = 2.5
        self.assertAlmostEqual(2.5, metrics.difficulty, places=6)
        # E = 2.5 * 20 = 50
        self.assertAlmostEqual(50.0, metrics.effort, places=6)

    def test_as_dict_keys(self):
        keys = set(HalsteadMetrics().as_dict())
        self.assertEqual(
            {'n1', 'n2', 'N1', 'N2', 'vocabulary', 'length',
             'calculated_length', 'volume', 'difficulty', 'effort',
             'time', 'bugs'},
            keys)


class TestHalsteadCpp(unittest.TestCase):
    """The generic classifier applied end-to-end to C++."""

    def test_simple_c_function(self):
        function = get_cpp_function(
            "int add(int a, int b) {\n"
            "    return a + b;\n"
            "}\n")
        operators = function._halstead_operators
        operands = function._halstead_operands
        # int (the two parameter types), the operators, and the braces.
        self.assertEqual(2, operators['int'])
        self.assertEqual(1, operators['+'])
        self.assertEqual(1, operators['return'])
        self.assertEqual(2, operands['a'])
        self.assertEqual(2, operands['b'])
        self.assertTrue(function.halstead.volume > 0)


class TestExtensionStatelessness(unittest.TestCase):

    def test_instance_holds_no_per_file_state(self):
        extension = HalsteadExtension()
        get_python_function_list_with_extension(SAMPLE, extension)
        self.assertEqual({}, extension.__dict__)

    def test_reused_instance_gives_independent_results(self):
        extension = HalsteadExtension()
        first = get_python_function_list_with_extension(
            SAMPLE, extension)[0]
        second = get_python_function_list_with_extension(
            "def g():\n    return 1\n", extension)[0]
        self.assertEqual(13, first.halstead.distinct_operators)
        self.assertEqual(1, second.halstead.distinct_operands)


if __name__ == '__main__':
    unittest.main()
