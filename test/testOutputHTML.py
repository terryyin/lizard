from mock import Mock, patch
import unittest
import sys
from lizard_ext import html_output
from lizard import parse_args, FunctionInfo, FileInformation, AllResult
from test.helper_stream import StreamStdoutTestCase


class TestHTMLOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.option = parse_args("app")
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        self.fileSummary = FileInformation("FILENAME", 123, [self.foo])
        self.scheme = Mock()

    def test_should_have_html_body(self):
        html_output([self.fileSummary], self.option, None, AllResult)
        self.assertRegex(sys.stdout.stream, r"\<html\>")

    def test_should_have_datatables_integration(self):
        html_output([self.fileSummary], self.option, None, AllResult)
        # Check for DataTables CSS
        self.assertIn("datatables.net", sys.stdout.stream)
        # Check for jQuery
        self.assertIn("jquery", sys.stdout.stream)
        # Check for DataTables initialization
        self.assertIn("DataTable(", sys.stdout.stream)
        # Check for table ID
        self.assertIn("complexityTable", sys.stdout.stream)

    def test_should_have_graceful_degradation(self):
        html_output([self.fileSummary], self.option, None, AllResult)
        # Check for graceful degradation checks
        self.assertIn("typeof jQuery", sys.stdout.stream)
        self.assertIn("typeof jQuery.fn.dataTable", sys.stdout.stream)
        # Check for try-catch error handling
        self.assertIn("try {", sys.stdout.stream)
        self.assertIn("catch (e)", sys.stdout.stream)
        # Check for fallback styling
        self.assertIn("table#complexityTable", sys.stdout.stream)
