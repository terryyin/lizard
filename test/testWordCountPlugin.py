import unittest
from test.mock import patch
from lizard_ext.lizardwordcount import LizardExtension


class FakeReader(object):

    class FI(object) : pass

    def __init__(self):
        self.fileinfo = self.FI()
        self.context = self
    def get_word_map(self):
        return self.fileinfo.wordCount


class TestWordCountPlugin(unittest.TestCase):

    def setUp(self):
        self.reader = FakeReader()
        self.ext = LizardExtension()

    def test_count_one_word(self):
        list(self.ext(["a", "b"], self.reader))
        self.assertEqual(1, self.reader.get_word_map()['a'])
        self.assertEqual(1, self.reader.get_word_map()['b'])

    def test_count_one_word_multiple_times(self):
        list(self.ext(["a", "a"], self.reader))
        self.assertEqual(2, self.reader.get_word_map()['a'])

    def test_count_one_word_multiple_times(self):
        list(self.ext(["a", "a"], self.reader))
        self.assertEqual(2, self.reader.get_word_map()['a'])

    def test_should_not_count_keywords(self):
        list(self.ext(["for"], self.reader))
        self.assertNotIn('for', self.reader.get_word_map())

    def test_should_count_non_keyword(self):
        list(self.ext(["For"], self.reader))
        self.assertIn('For', self.reader.get_word_map())

    def test_should_not_count_string(self):
        list(self.ext(["\"\""], self.reader))
        self.assertEqual(0, len(self.reader.get_word_map()))

    def test_should_not_line_continuer(self):
        list(self.ext(["\\\n"], self.reader))
        self.assertEqual(0, len(self.reader.get_word_map()))

    def test_reduce_the_result(self):
        list(self.ext(["a"], self.reader))
        self.ext.reduce(self.reader.fileinfo)
        self.ext.reduce(self.reader.fileinfo)
        self.assertEqual(2, self.ext.result['a'])

class TestWordCountOutput(unittest.TestCase):

    def setUp(self):
        self.buf = ''

    def write_to_buffer(self, txt):
            self.buf += txt

    @patch('webbrowser.open')
    @patch('lizard_ext.lizardwordcount.open', create=True)
    def test_should_output_html(self, mock_open, browser_open):
        buf = ""
        mock_open.return_value.__enter__.return_value.write.side_effect = self.write_to_buffer
        ext = LizardExtension()
        ext.result = {'a':123}
        ext.print_result()
        mock_open.assert_called_once_with('codecloud.html', 'w')
        self.assertIn('<html>', self.buf)
        self.assertIn('["a", 123]', self.buf)

    @patch('webbrowser.open')
    @patch('lizard_ext.lizardwordcount.open', create=True)
    def test_should_open_the_browser(self, mock_open, browser_open):
        import os
        ext = LizardExtension()
        ext.result = {'a':123}
        ext.print_result()
        browser_open.assert_called_with('file://' + os.path.abspath('codecloud.html'));

