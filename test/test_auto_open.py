# coding=utf-8
import unittest
import codecs
import os
from mock import patch, Mock
from tempfile import NamedTemporaryFile
from lizard_ext import auto_open, auto_read


class TestAutoOpen(unittest.TestCase):

    def write_and_read(self, encoding, content):
        f = NamedTemporaryFile(delete=False)
        filename = f.name
        f.close()

        with codecs.open(filename, 'w+b', encoding=encoding) as fh:
            fh.write(content)
            fh.flush()
            handle = auto_open(filename, 'rU')
            result = handle.read()

        os.unlink(filename)
        return result

    def test_ascii(self):
        result = self.write_and_read("ascii", "abcd123")
        self.assertEqual("abcd123", result)

    def test_utf_8(self):
        result = self.write_and_read("utf-8", u"天下太平")
        self.assertEqual("天下太平", result)

    def test_utf_8_with_bom(self):
        result = self.write_and_read("utf-8-sig", u"天下太平")
        self.assertEqual(u"天下太平", result)


class TestAutoRead(unittest.TestCase):

    def test_at(self):
        result = auto_read(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'data/utf.c'))
        self.assertIn("after", result)
