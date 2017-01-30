import unittest
from lizard_languages import get_reader_for, CLikeReader, JavaReader, ObjCReader, JavaScriptReader, ScalaReader, GDScriptReader


class TestLanguageChooser(unittest.TestCase):

    def test_not_case_sensitive(self):
        self.assertEqual(CLikeReader, get_reader_for("a.Cpp"))

    def test_java(self):
        self.assertEqual(JavaReader, get_reader_for("a.java"))

    def test_objectiveC(self):
        self.assertEqual(ObjCReader, get_reader_for("a.m"))

    def test_c_cpp(self):
        for name in ("a.cpp", ".cxx", ".h", ".hpp"):
            self.assertEqual(CLikeReader, get_reader_for(name),
                             "File name '%s' is not recognized as c/c++ file" % name)

    def test_JavaScript(self):
        self.assertEqual(JavaScriptReader, get_reader_for("a.js"))

    def test_scala(self):
        self.assertEqual(ScalaReader, get_reader_for("a.scala"))

    def test_GDScript(self):
        self.assertEqual(GDScriptReader, get_reader_for("a.gd"))

    def test_unknown_extension(self):
        self.assertEqual(None, get_reader_for("a.unknown"))

