from mock import Mock, patch
import unittest
import sys
from lizard_ext import html_output
from lizard import parse_args, FunctionInfo, FileInformation
from test.helper_stream import StreamStdoutTestCase


class TestHTMLOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.option = parse_args("app")
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        self.fileSummary = FileInformation("FILENAME", 123, [self.foo])
        self.scheme = Mock()

    def test_should_have_html_body(self):
        html_output([self.fileSummary], self.option, None)
        self.assertRegexpMatches(sys.stdout.stream,
                r"\<html\>")
