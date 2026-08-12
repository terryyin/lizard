from unittest.mock import Mock, patch
import unittest
import sys
from lizard_ext import csv_output
from test.helper_stream import StreamStdoutTestCase
from lizard import parse_args, print_and_save_modules, FunctionInfo, FileInformation,\
    get_extensions, OutputScheme, AllResult


class TestCSVOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.option = parse_args("app")
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        self.fileSummary = FileInformation("FILENAME", 123, [self.foo])
        self.extensions = get_extensions([])
        self.scheme = OutputScheme(self.extensions)

    def test_csv_header(self):
        options_mock = Mock()
        options_mock.verbose = True
        options_mock.extensions = []
        csv_output(AllResult([self.fileSummary]), options_mock)
        self.assertRegex(sys.stdout.stream,
                                 r"NLOC,CCN,token,PARAM,length,location,file,function,long_name,start,end")

    def test_csv_header_with_extension(self):
        options_mock = Mock()
        options_mock.verbose = True
        extension_mock = Mock()
        extension_mock.__class__.__name__ = 'LizardExtension'
        extension_mock.FUNCTION_INFO = {"exit_count": {"caption": "exits"}}
        options_mock.extensions = [extension_mock]
        results = AllResult([self.fileSummary])
        results.result[0].function_list[0].exit_count = 1
        csv_output(results, options_mock)
        self.assertRegex(sys.stdout.stream,
                                 r"NLOC,CCN,token,PARAM,length,location,file,function,long_name,start,end,exits")

    def test_csv_no_header(self):
        options_mock = Mock()
        options_mock.verbose = False
        options_mock.extensions = []
        csv_output(AllResult([self.fileSummary]), options_mock)
        self.assertEqual(
            '1,1,1,0,1,"foo@100-100@FILENAME","FILENAME","foo","foo",100,100',
            sys.stdout.stream.splitlines()[0]
        )

    def test_csv_no_header_with_extension(self):
        options_mock = Mock()
        options_mock.verbose = False
        options_mock.extensions = []
        extension_mock = Mock()
        extension_mock.__class__.__name__ = 'LizardExtension'
        extension_mock.FUNCTION_INFO = {"exit_count": {"caption": "exits"}}
        options_mock.extensions = [extension_mock]

        results = AllResult([self.fileSummary])
        results.result[0].function_list[0].exit_count = 1
        csv_output(results, options_mock)

        self.assertEqual(
            '1,1,1,0,1,"foo@100-100@FILENAME","FILENAME","foo","foo",100,100,1',
            sys.stdout.stream.splitlines()[0]
        )

    def test_csv_header_with_multi_field_extension(self):
        options_mock = Mock()
        options_mock.verbose = True
        extension_mock = Mock()
        extension_mock.__class__.__name__ = 'LizardExtension'
        extension_mock.FUNCTION_INFO = {
            "metric_one": {"caption": " metric_one "},
            "metric_two": {"caption": " metric_two "},
        }
        options_mock.extensions = [extension_mock]
        results = AllResult([self.fileSummary])
        results.result[0].function_list[0].metric_one = 12.5
        results.result[0].function_list[0].metric_two = 99.0
        csv_output(results, options_mock)
        self.assertRegex(
            sys.stdout.stream,
            r"NLOC,CCN,token,PARAM,length,location,file,function,"
            r"long_name,start,end, metric_one , metric_two ")

    def test_csv_row_with_multi_field_extension(self):
        options_mock = Mock()
        options_mock.verbose = False
        extension_mock = Mock()
        extension_mock.__class__.__name__ = 'LizardExtension'
        extension_mock.FUNCTION_INFO = {
            "metric_one": {"caption": " metric_one "},
            "metric_two": {"caption": " metric_two "},
        }
        options_mock.extensions = [extension_mock]
        results = AllResult([self.fileSummary])
        results.result[0].function_list[0].metric_one = 12.5
        results.result[0].function_list[0].metric_two = 99.0
        csv_output(results, options_mock)
        self.assertEqual(
            '1,1,1,0,1,"foo@100-100@FILENAME","FILENAME","foo","foo",'
            '100,100,12.5,99.0',
            sys.stdout.stream.splitlines()[0]
        )

    def test_print_fileinfo(self):
        options_mock = Mock()
        options_mock.verbose = True
        options_mock.extensions = []

        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 16
        file_stat = FileInformation("FILENAME", 1, [self.foo])

        csv_output(AllResult([file_stat]), options_mock)
        self.assertEqual(
            '1,16,1,0,1,"foo@100-100@FILENAME","FILENAME","foo","foo",100,100',
            sys.stdout.stream.splitlines()[1]
        )

    def test_print_extension(self):
        options_mock = Mock()
        options_mock.verbose = True
        options_mock.extensions = []

        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 16
        file_stat = FileInformation("FILENAME", 1, [self.foo])

        csv_output(AllResult([file_stat]), options_mock)

        self.assertEqual(
            '1,16,1,0,1,"foo@100-100@FILENAME","FILENAME","foo","foo",100,100',
            sys.stdout.stream.splitlines()[1]
        )
