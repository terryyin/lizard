# coding=utf-8
import unittest
import codecs
import io
import os
from tempfile import NamedTemporaryFile
from lizard_ext import auto_open, auto_read


class TestAutoOpen(unittest.TestCase):

    def write_and_read(self, encoding, content):
        with NamedTemporaryFile() as f:
            filename = f.name
        with io.open(filename, 'w+b') as fh:
            fh.write(content)
            fh.flush()
        with auto_open(filename, 'rt', encoding=encoding) as handle:
            result = handle.read()
        os.unlink(filename)
        return result

    def test_ascii(self):
        result = self.write_and_read("ascii", "abcd123".encode('ascii'))
        self.assertEqual("abcd123", result)

    def test_utf_8(self):
        result = self.write_and_read("utf-8", u"天下太平".encode('utf-8'))
        self.assertEqual(u"天下太平", result)

    def test_utf_8_with_bom(self):
        binary = codecs.BOM_UTF8 + u"天下太平".encode('utf-8')
        result = self.write_and_read("utf-8-sig", binary)
        self.assertEqual(u"天下太平", result)

    def test_utf_8_with_unexpected_bom(self):
        binary = codecs.BOM_UTF8 + u"天下太平".encode('utf-8')
        result = self.write_and_read("utf-8", binary)
        self.assertEqual(u"天下太平", result)


class TestAutoRead(unittest.TestCase):

    def test_at(self):
        result = auto_read(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'data/utf.c'))
        self.assertIn("after", result)
