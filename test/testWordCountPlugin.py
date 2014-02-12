import unittest
from lizardWordCount import LizardExtension


class TestWordCountPlugin(unittest.TestCase):

    def test_count_one_word(self):
        ext = LizardExtension()
        list(ext.extend_tokens(["a", "b"]))
        self.assertEqual(1, ext.result['a'])
        self.assertEqual(1, ext.result['b'])

    def test_count_one_word_multiple_times(self):
        ext = LizardExtension()
        list(ext.extend_tokens(["a", "a"]))
        self.assertEqual(2, ext.result['a'])

    def test_should_not_count_keywords(self):
        ext = LizardExtension()
        list(ext.extend_tokens(["for"]))
        self.assertNotIn('for', ext.result)

    def test_should_count_non_keyword(self):
        ext = LizardExtension()
        list(ext.extend_tokens(["For"]))
        self.assertIn('For', ext.result)

