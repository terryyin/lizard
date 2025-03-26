import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
import os


class TestPerl(unittest.TestCase):

    def test_perl_function(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl1.pl"))
        self.assertEqual(1, len(result.function_list))
        self.assertEqual('sub_with_complexity', result.function_list[0].name)
        self.assertEqual(2, result.function_list[0].cyclomatic_complexity)

    def test_perl_forgive_comment(self):
        result = analyze_file(os.path.join(os.path.dirname(__file__), "testdata/perl_forgive.pl"))
        self.assertEqual(0, len(result.function_list))  # Function should be forgiven

    def test_perl_forgive_comment_variations(self):
        test_cases = [
            "# #lizard forgives",
            "#lizard forgives",
            "##lizard forgives",
            "###lizard forgives",
            "# lizard forgives",
        ]
        
        for comment in test_cases:
            code = f"""
{comment}
sub complex_sub {{
    my ($x) = @_;
    if ($x > 0) {{
        print "Positive\\n";
    }}
    return $x;
}}
"""
            with open("test.pl", "w") as f:
                f.write(code)
            try:
                result = analyze_file("test.pl")
                self.assertEqual(0, len(result.function_list), 
                               f"Failed for comment format: {comment}")
            finally:
                if os.path.exists("test.pl"):
                    os.remove("test.pl")

    def test_perl_forgive_inside_sub(self):
        code = """
sub complex_sub {
    # #lizard forgives
    my ($x) = @_;
    if ($x > 0) {
        print "Positive\\n";
    }
    return $x;
}
"""
        with open("test.pl", "w") as f:
            f.write(code)
        try:
            result = analyze_file("test.pl")
            self.assertEqual(0, len(result.function_list))
        finally:
            if os.path.exists("test.pl"):
                os.remove("test.pl") 