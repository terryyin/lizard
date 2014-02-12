import unittest
from lizardWordCount import LizardExtension


class Context(object):

    class FI(object) : pass

    def __init__(self):
        self.fileinfo = self.FI()
    def get_word_map(self):
        return self.fileinfo.wordCount


class TestWordCountPlugin(unittest.TestCase):

    def setUp(self):
        self.context = Context()
    def test_count_one_word(self):
        ext = LizardExtension()
        list(ext.extend_tokens(["a", "b"], self.context))
        self.assertEqual(1, self.context.get_word_map()['a'])
        self.assertEqual(1, self.context.get_word_map()['b'])

    def test_count_one_word_multiple_times(self):
        ext = LizardExtension()
        list(ext.extend_tokens(["a", "a"], self.context))
        self.assertEqual(2, self.context.get_word_map()['a'])

    def test_should_not_count_keywords(self):
        ext = LizardExtension()
        list(ext.extend_tokens(["for"], self.context))
        self.assertNotIn('for', self.context.get_word_map())

    def test_should_count_non_keyword(self):
        ext = LizardExtension()
        list(ext.extend_tokens(["For"], self.context))
        self.assertIn('For', self.context.get_word_map())

