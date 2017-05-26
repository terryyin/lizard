from mock import Mock, patch
import unittest
import sys
from lizard_ext import csv_output
from test.helper_stream import StreamStdoutTestCase
from lizard import parse_args, print_and_save_modules, FunctionInfo, FileInformation,\
    get_extensions, OutputScheme


class TestCSVOutput(StreamStdoutTestCase):


    def setUp(self):
        StreamStdoutTestCase.setUp(self)
        self.option = parse_args("app")
        self.foo = FunctionInfo("foo", 'FILENAME', 100)
        self.fileSummary = FileInformation("FILENAME", 123, [self.foo])
        self.extensions = get_extensions([])
        self.scheme = OutputScheme(self.extensions)


    def test_csv_header(self):
        csv_output([self.fileSummary], True)
        self.assertRegexpMatches(sys.stdout.stream,
                                 r"NLOC,CCN,token,PARAM,length,location,file,function,start,end")


    def test_csv_no_header(self):
        csv_output([self.fileSummary], False)
        self.assertEquals(
            '1,1,1,0,0,"foo@100-100@FILENAME","FILENAME","foo",100,100',
            sys.stdout.stream.splitlines()[0]
        )


    def test_print_fileinfo(self):
        self.foo.end_line = 100
        self.foo.cyclomatic_complexity = 16
        fileStat = FileInformation("FILENAME", 1, [self.foo])

        csv_output([fileStat], True)

        self.assertEquals(
            '1,16,1,0,0,"foo@100-100@FILENAME","FILENAME","foo",100,100',
            sys.stdout.stream.splitlines()[1]
        )
