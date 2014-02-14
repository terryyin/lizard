import unittest
from test.mock import patch
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
        self.ext = LizardExtension()

    def test_count_one_word(self):
        list(self.ext.extend_tokens(["a", "b"], self.context))
        self.assertEqual(1, self.context.get_word_map()['a'])
        self.assertEqual(1, self.context.get_word_map()['b'])

    def test_count_one_word_multiple_times(self):
        list(self.ext.extend_tokens(["a", "a"], self.context))
        self.assertEqual(2, self.context.get_word_map()['a'])

    def test_should_not_count_keywords(self):
        list(self.ext.extend_tokens(["for"], self.context))
        self.assertNotIn('for', self.context.get_word_map())

    def test_should_count_non_keyword(self):
        list(self.ext.extend_tokens(["For"], self.context))
        self.assertIn('For', self.context.get_word_map())

    def test_should_not_count_string(self):
        list(self.ext.extend_tokens(["\"\""], self.context))
        self.assertEqual(0, len(self.context.get_word_map()))

    def test_reduce_the_result(self):
        list(self.ext.extend_tokens(["a"], self.context))
        self.ext.reduce(self.context.fileinfo)
        self.ext.reduce(self.context.fileinfo)
        self.assertEqual(2, self.ext.result['a'])

class TestWordCountOutput(unittest.TestCase):

    def setUp(self):
        self.context = Context()
        self.buf = ''

    def write_to_buffer(self, txt):
            self.buf += txt

    @patch('lizardWordCount.open', create=True)
    def test_should_output_html(self, mock_open):
        buf = ""
        mock_open.return_value.__enter__.return_value.write.side_effect = self.write_to_buffer
        ext = LizardExtension()
        ext.result = {'a':123}
        ext.print_result()
        mock_open.assert_called_once_with('codecloud.html', 'w')
        self.assertIn('<html>', self.buf)
        self.assertIn('["a", 123]', self.buf)
