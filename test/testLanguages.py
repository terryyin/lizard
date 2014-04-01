import unittest
from lizard import Languages, CodeReader


class TestLanguageChooser(unittest.TestCase):

    def test_not_case_sensitive(self):
        self.assertEqual("c/c++", Languages().get_reader("a.Cpp").lan)

    def test_java(self):
        self.assertEqual("Java", Languages().get_reader("a.java").lan)

    def test_objectiveC(self):
        self.assertEqual("objC", Languages().get_reader("a.m").lan )

    def test_c_cpp(self):
        for name in ("a.cpp", ".cxx", ".h", ".hpp"):
            self.assertEqual("c/c++", Languages().get_reader(name).lan,
                             "File name '%s' is not recognized as c/c++ file" % name);

    def test_unknown_extension(self):
        self.assertEqual(None, Languages().get_reader("a.unknown"));

    def test_new_reader_should_be_found(self):
        class NewReader(CodeReader):
            lan = 'new'
            ext = ['ext']

        self.assertEqual(NewReader, Languages().get_reader("a.ext"));
        del NewReader


