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

    def test_html_output_with_extension(self):
        """Test that using --html with .html extension works correctly without warnings"""
        path = join(self.tmp_dir, "test.html")
        args = ["lizard", "--length", "75", "--CCN", "20", "--arguments", "3",
               "--warnings_only", "--sort", "cyclomatic_complexity", "--html",
               "--output_file", path, "test/data"]
        
        # Capture stdout to check for warnings
        import sys
        from io import StringIO
        stdout = StringIO()
        old_stdout = sys.stdout
        sys.stdout = stdout
        
        try:
            main(args)
            output = stdout.getvalue()
            self.assertNotIn("Warning: overriding output file extension", output,
                           "Should not show extension warning when using .html extension with --html")
            
            # Verify the file exists and has HTML content
            with open(path, 'r') as f:
                content = f.read()
                self.assertTrue(content.startswith("<!DOCTYPE HTML PUBLIC"),
                              "Output file should contain HTML content")
        finally:
            sys.stdout = old_stdout

    def test_html_extension_warning(self):
        """Test that using --html with .html extension works correctly without warnings,
        but using a different format with .html extension shows a warning"""
        path = join(self.tmp_dir, "test.html")
        
        # Capture stderr to check for warnings
        import sys
        from io import StringIO
        stderr = StringIO()
        old_stderr = sys.stderr
        sys.stderr = stderr
        
        try:
            # First test: --html with .html extension should not warn
            args = ["lizard", "--html", "--output_file", path, "test/data"]
            main(args)
            output = stderr.getvalue()
            self.assertNotIn("Warning: overriding output file extension", output,
                           "Should not show warning when format matches extension")
            stderr.truncate(0)
            stderr.seek(0)
            
            # Second test: --xml with .html extension should warn
            args = ["lizard", "--xml", "--output_file", path, "test/data"]
            main(args)
            output = stderr.getvalue()
            self.assertIn("Warning: overriding output file extension", output,
                         "Should show warning when format doesn't match extension")
        finally:
            sys.stderr = old_stderr

    def test_checkstyle(self):
        header = "<?xml version=\"1.0\" ?>"
        # Checkstyle output should start with XML declaration
        self.output_test("test.checkstyle.xml", header)

    def test_checkstyle_option(self):
        # Create a temp C file with a function
        c_path = join(self.tmp_dir, "foo.c")
        with open(c_path, "w") as f:
            f.write("int foo() { return 42; }\n")
        path = join(self.tmp_dir, "test.checkstyle.xml")
        args = ["lizard", "--checkstyle", "--output_file", path, c_path]
        main(args)
        with open(path, 'r') as f:
            content = f.read()
            self.assertIn('<checkstyle', content)
            self.assertIn('<file', content)
            self.assertIn('<error', content)

    def test_checkstyle_extension_inference(self):
        # Create a temp C file with a function
        c_path = join(self.tmp_dir, "bar.c")
        with open(c_path, "w") as f:
            f.write("int bar() { return 24; }\n")
        path = join(self.tmp_dir, "test.checkstyle.xml")
        args = ["lizard", "--output_file", path, c_path]
        main(args)
        with open(path, 'r') as f:
            content = f.read()
            self.assertIn('<checkstyle', content)
