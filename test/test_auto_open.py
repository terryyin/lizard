# -*- coding: utf-8 -*-
import unittest
from mock import patch, Mock
from tempfile import NamedTemporaryFile
from lizard_ext import auto_open


class TestAutoOpen(unittest.TestCase):

    def write_and_read(self, encoding, content):
        with NamedTemporaryFile("w", encoding=encoding) as nf:
            nf.write(content)
            nf.flush()
            handle = auto_open(nf.name, 'rU')
            return handle.read()

    def test_ascii(self):
        result = self.write_and_read("ascii", "abcd123")
        self.assertEqual("abcd123", result)

    def test_utf_8(self):
        result = self.write_and_read("utf-8", u"天下太平")
        self.assertEqual(u"天下太平", result)

    def test_utf_8_with_bom(self):
        result = self.write_and_read("utf-8-sig", u"天下太平")
        self.assertEqual(u"天下太平", result)
