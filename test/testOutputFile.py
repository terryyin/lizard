from os.path import join
from shutil import rmtree
from tempfile import mkdtemp
import unittest
from lizard import html_output, print_xml, parse_args, main


class TestFileOutputArgParsing(unittest.TestCase):

    def test_short(self):
        args = parse_args(["lizard", "-o test_file"])
        self.assertEqual("test_file", args.output_file.strip())

    def test_long(self):
        args = parse_args(["lizard", "--output_file", "test_file"])
        self.assertEqual("test_file", args.output_file.strip())

    def test_standalone(self):
        args = parse_args(["lizard", "--output_file", "test_file.html"])
        self.assertEqual(html_output, args.printer)

    def test_override(self):
        args = parse_args(["lizard", "--output_file test_file.html", "--xml"])
        self.assertEqual(print_xml, args.printer)


class TestFileOutputIntegration(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = mkdtemp()
        print("Tmp directory '{}' created.\n".format(self.tmp_dir))

    def tearDown(self):
        rmtree(self.tmp_dir)
        print("Tmp directory '{}' deleted.\n".format(self.tmp_dir))

    def output_test(self, file_name, expected_first_line):
        path = join(self.tmp_dir, file_name)
        args = ["lizard", "--verbose", "--output_file", path, "test/data"]
        main(args)
        first_line = open(path, 'r').readline().strip('\n')
        self.assertEqual(first_line, expected_first_line)

    def test_default(self):
        header = "================================================"
        self.output_test("test", header)

    def test_csv(self):
        header = "NLOC,CCN,token,PARAM,length,location,file,function,long_name,start,end"
        self.output_test("test.csv", header)

    def test_html(self):
        header = "<!DOCTYPE HTML PUBLIC"
        self.output_test("test.html", header)

    def test_xml(self):
        header = "<?xml version=\"1.0\" ?>"
        self.output_test("test.xml", header)
