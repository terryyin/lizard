from mock import Mock, patch
import unittest
import sys
import json
from lizard_ext import json_output
from test.helper_stream import StreamStdoutTestCase
from lizard import parse_args, print_and_save_modules, FunctionInfo, FileInformation, \
    get_extensions, OutputScheme


class TestCSVOutput(StreamStdoutTestCase):


    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.option = parse_args("app")
        self.foo = FunctionInfo("foo", "FILENAME", 100)
        self.fileSummary = FileInformation("FILENAME", 123, [self.foo])
        self.extensions = get_extensions([])
        self.scheme = OutputScheme(self.extensions)


    def test_print_fileinfo(self):
        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 16
        fileStat = FileInformation("FILENAME", 1, [self.foo])

        json_output([fileStat], False)

        json_object = json.loads(sys.stdout.stream)

        self.assertTrue("header" in json_object)
        self.assertTrue("files"  in json_object)

        self.assertTrue(len(json_object["files"]) == 1)

        self.assertTrue(json_object["files"][0]["NLOC"] == 1)
        self.assertTrue(json_object["files"][0]["CCN"] == 16)
        self.assertTrue(json_object["files"][0]["token"] == 1)
        self.assertTrue(json_object["files"][0]["PARAM"] == 0)
        self.assertTrue(json_object["files"][0]["length"] == 0)
        self.assertTrue(json_object["files"][0]["location"] == "foo@100-100@FILENAME")
        self.assertTrue(json_object["files"][0]["file"] == "FILENAME")
        self.assertTrue(json_object["files"][0]["function"] == "foo")
        self.assertTrue(json_object["files"][0]["start"] == 100)
        self.assertTrue(json_object["files"][0]["end"] == 100)
