from unittest.mock import Mock
import sys
from lizard_ext import csv_output
from lizard_ext.lizardio import LizardExtension as IoExtension
from test.helper_stream import StreamStdoutTestCase
from lizard import FunctionInfo, FileInformation, AllResult


class TestCSVOutput(StreamStdoutTestCase):

    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        self.fileSummary = FileInformation("FILENAME", 123, [self.foo])

    def _options(self, extensions=None, verbose=False):
        options = Mock()
        options.verbose = verbose
        options.extensions = extensions or []
        return options

    def _results_with_attrs(self, **attrs):
        results = AllResult([self.fileSummary])
        function = results.result[0].function_list[0]
        for name, value in attrs.items():
            setattr(function, name, value)
        return results

    def test_csv_header(self):
        csv_output(AllResult([self.fileSummary]), self._options(verbose=True))
        self.assertRegex(
            sys.stdout.stream,
            r"NLOC,CCN,token,PARAM,length,location,file,function,"
            r"long_name,start,end")

    def test_csv_header_with_extension(self):
        extension = Mock()
        extension.FUNCTION_INFO = {"exit_count": {"caption": "exits"}}
        csv_output(
            self._results_with_attrs(exit_count=1),
            self._options([extension], verbose=True))
        self.assertRegex(
            sys.stdout.stream,
            r"NLOC,CCN,token,PARAM,length,location,file,function,"
            r"long_name,start,end,exits")

    def test_csv_no_header(self):
        csv_output(AllResult([self.fileSummary]), self._options())
        self.assertEqual(
            '1,1,1,0,1,"foo@100-100@FILENAME","FILENAME","foo","foo",'
            '100,100',
            sys.stdout.stream.splitlines()[0]
        )

    def test_csv_no_header_with_extension(self):
        extension = Mock()
        extension.FUNCTION_INFO = {"exit_count": {"caption": "exits"}}
        csv_output(
            self._results_with_attrs(exit_count=1),
            self._options([extension]))
        self.assertEqual(
            '1,1,1,0,1,"foo@100-100@FILENAME","FILENAME","foo","foo",'
            '100,100,1',
            sys.stdout.stream.splitlines()[0]
        )

    def test_csv_header_with_multi_field_extension(self):
        csv_output(
            self._results_with_attrs(
                fan_in=1, fan_out=2, general_fan_out=3),
            self._options([IoExtension()], verbose=True))
        self.assertRegex(
            sys.stdout.stream,
            r"NLOC,CCN,token,PARAM,length,location,file,function,"
            r"long_name,start,end, fan_in , fan_out , general_fan_out ")

    def test_csv_row_with_multi_field_extension(self):
        csv_output(
            self._results_with_attrs(
                fan_in=1, fan_out=2, general_fan_out=3),
            self._options([IoExtension()]))
        self.assertEqual(
            '1,1,1,0,1,"foo@100-100@FILENAME","FILENAME","foo","foo",'
            '100,100,1,2,3',
            sys.stdout.stream.splitlines()[0]
        )

    def test_print_fileinfo(self):
        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 16
        file_stat = FileInformation("FILENAME", 1, [self.foo])

        csv_output(AllResult([file_stat]), self._options(verbose=True))
        self.assertEqual(
            '1,16,1,0,1,"foo@100-100@FILENAME","FILENAME","foo","foo",'
            '100,100',
            sys.stdout.stream.splitlines()[1]
        )
