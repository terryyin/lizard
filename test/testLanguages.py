import unittest
from lizard import CodeReader, CLikeReader, JavaReader, ObjCReader, JavaScriptReader


class TestLanguageChooser(unittest.TestCase):

    def test_not_case_sensitive(self):
        self.assertEqual(CLikeReader, CodeReader.get_reader("a.Cpp"))

    def test_java(self):
        self.assertEqual(JavaReader, CodeReader.get_reader("a.java"))

    def test_objectiveC(self):
        self.assertEqual(ObjCReader, CodeReader.get_reader("a.m"))

    def test_c_cpp(self):
        for name in ("a.cpp", ".cxx", ".h", ".hpp"):
            self.assertEqual(CLikeReader, CodeReader.get_reader(name),
                             "File name '%s' is not recognized as c/c++ file" % name);

    def test_JavaScript(self):
        self.assertEqual(JavaScriptReader, CodeReader.get_reader("a.js"))

    def test_unknown_extension(self):
        self.assertEqual(None, CodeReader.get_reader("a.unknown"));

    def test_new_reader_should_be_found(self):
        class NewReader(CodeReader):
            ext = ['ext']

        self.assertEqual(NewReader, CodeReader.get_reader("a.ext"));
        del NewReader


