from os.path import join
from shutil import rmtree
from tempfile import mkdtemp
import unittest
from lizard import html_output, print_xml, md5_hash_file, parse_args, main


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

    def output_test(self, file_name, expected_md5=None):
        path = join(self.tmp_dir, file_name)
        args = ["lizard", "--verbose", "--output_file", path, "test/data"]
        main(args)
        if expected_md5:
            computed_md5 = md5_hash_file(path)
            self.assertEqual(expected_md5, computed_md5)

    def test_default(self):
        self.output_test("test", "eac0d9e24290640a281a5a87c6118acc")

    def test_csv(self):
        self.output_test("test.csv", "4e808054b31ffbd90e93282f6dd3e95f")

    def test_html(self):
        # No MD5 check for HTML output, because it is not reproducible
        # (includes a timestamp).
        self.output_test("test.html")

    def test_xml(self):
        self.output_test("test.xml", "8338082a4bcf8faaa89a05503a85b315")
