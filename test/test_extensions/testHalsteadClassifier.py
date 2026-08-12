"""Unit tests for Halstead token classification and classifier selection."""

import unittest

from lizard_ext.halstead_classifier import (
    HalsteadClassifier,
    PythonHalsteadClassifier,
    get_classifier,
)


class TestHalsteadClassifier(unittest.TestCase):

    def setUp(self):
        self.python = PythonHalsteadClassifier()
        self.generic = HalsteadClassifier()

    def test_python_keyword_operators(self):
        for token in ('if', 'else', 'for', 'return', 'and', 'or', 'not',
                      'def', 'class', 'import', 'lambda', 'yield'):
            self.assertEqual(HalsteadClassifier.OPERATOR,
                             self.python.classify(token), token)

    def test_python_soft_keywords_are_operands(self):
        # match/case/type/_ are context-dependent; treat them as identifiers.
        for token in ('match', 'case', 'type', '_'):
            self.assertEqual(HalsteadClassifier.OPERAND,
                             self.python.classify(token), token)

    def test_identifiers_and_symbols(self):
        for token in ('foo', 'bar_baz', '_private', 'CamelCase'):
            self.assertEqual(HalsteadClassifier.OPERAND,
                             self.python.classify(token), token)
        for token in ('+', '-', '*', '==', '!=', '(', ')', '{', '}',
                      ':', ',', '.', '&&'):
            self.assertEqual(HalsteadClassifier.OPERATOR,
                             self.python.classify(token), token)

    def test_leading_dot_float_is_operand(self):
        # Some tokenizers (e.g. C-family) emit ``.5`` as a single token.
        for token in ('.5', '.5e3', '.25E-4', '0.5', '5.'):
            self.assertEqual(HalsteadClassifier.OPERAND,
                             self.generic.classify(token), token)

    def test_bare_dot_and_ellipsis_stay_operators_for_generic(self):
        # In C-family code ``...`` is pack-expansion punctuation, not a literal.
        for token in ('.', '..', '...', '.foo'):
            self.assertEqual(HalsteadClassifier.OPERATOR,
                             self.generic.classify(token), token)

    def test_python_ellipsis_is_operand(self):
        self.assertEqual(HalsteadClassifier.OPERAND,
                         self.python.classify('...'))

    def test_python_string_prefix_before_string_is_skipped(self):
        for prefix in ('f', 'b', 'r', 'rf', 'fr', 'F', 'B'):
            self.assertIs(
                HalsteadClassifier.SKIP,
                self.python.classify_with_next(prefix, '"hi"'),
                prefix)

    def test_python_string_prefix_without_string_is_operand(self):
        for prefix in ('f', 'b', 'r'):
            self.assertEqual(
                HalsteadClassifier.OPERAND,
                self.python.classify_with_next(prefix, '+'),
                prefix)
            self.assertEqual(
                HalsteadClassifier.OPERAND,
                self.python.classify_with_next(prefix, None),
                prefix)

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
