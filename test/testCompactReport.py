import os
import re
import subprocess
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
import lizard


class TestCompactReportCli(unittest.TestCase):
    """Real CLI checks for the compact function table."""

    SUM = "const sum = array => array.reduce((a, b) => a + b);\n"

    def run_lizard(self, source, *args):
        lizard_py = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lizard.py"
        )
        with TemporaryDirectory() as directory:
            js_path = Path(directory) / "code.js"
            js_path.write_text(source)
            completed = subprocess.run(
                [sys.executable, lizard_py, *args, "code.js"],
                capture_output=True,
                text=True,
                cwd=directory,
            )
        self.assertEqual("", completed.stderr)
        return completed

    def function_rows(self, output):
        table = output.split(" file analyzed.")[0]
        return [
            line.split()[-1].split("@")[0]
            for line in table.splitlines()
            if "@code.js" in line
        ]

    def test_compact_omits_trivial_nested_callback_but_counts_it(self):
        compact = self.run_lizard(self.SUM, "--compact")
        default = self.run_lizard(self.SUM)
        self.assertEqual(["sum"], self.function_rows(compact.stdout))
        self.assertEqual(["(anonymous)", "sum"], self.function_rows(default.stdout))
        self.assertIn("         2     code.js", compact.stdout)
        self.assertEqual(
            default.stdout.split("file analyzed.")[1],
            compact.stdout.split("file analyzed.")[1],
        )
        self.assertEqual(0, compact.returncode)

    def test_compact_keeps_nested_callback_with_decisions(self):
        compact = self.run_lizard(
            "const sum = array => array.reduce((a, b) => a ? a + b : b);\n", "--compact"
        )
        self.assertEqual(["(anonymous)", "sum"], self.function_rows(compact.stdout))

    def test_compact_keeps_named_and_top_level_callbacks(self):
        compact = self.run_lizard(
            "function f() { function g() {} }\n"
            "[1, 2].forEach(x => console.log(x));\n",
            "--compact",
        )
        self.assertEqual(["g", "f", "(anonymous)"], self.function_rows(compact.stdout))

    def test_compact_keeps_warned_callback_and_exit_status(self):
        compact = self.run_lizard(self.SUM, "--compact", "-a", "1")
        self.assertEqual(["(anonymous)", "sum"], self.function_rows(compact.stdout))
        warnings = compact.stdout.split("!!!! Warnings")[1]
        self.assertIn("(anonymous)@1-1@code.js", warnings)
        self.assertEqual(1, compact.returncode)

    def test_compact_leaves_other_formatters_unchanged(self):
        # HTML embeds wall-clock minute precision; two CLI runs can straddle a
        # minute boundary without --compact changing the report.
        html_minute_stamp = re.compile(r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}")

        for formatter in (
            ["--xml"],
            ["--csv"],
            ["--checkstyle"],
            ["--html"],
            ["-w", "-a", "1"],
            ["--warning-msvs", "-a", "1"],
        ):
            with self.subTest(formatter=formatter):
                without_compact = self.run_lizard(self.SUM, *formatter).stdout
                with_compact = self.run_lizard(self.SUM, *formatter, "--compact").stdout
                if formatter == ["--html"]:
                    without_compact = html_minute_stamp.sub(
                        "TIMESTAMP", without_compact
                    )
                    with_compact = html_minute_stamp.sub("TIMESTAMP", with_compact)
                self.assertEqual(without_compact, with_compact)

    def test_compact_leaves_extension_printer_unchanged(self):
        def extension_output(*args):
            output = self.run_lizard(self.SUM * 2, "-Eduplicate", *args).stdout
            return output.split("Total nloc")[1]

        self.assertIn("Duplicate", extension_output())
        self.assertEqual(extension_output(), extension_output("--compact"))

    def test_analysis_results_keep_trivial_nested_callback(self):
        functions = lizard.analyze_file.analyze_source_code(
            "code.js", self.SUM
        ).function_list
        self.assertEqual(
            [("(anonymous)", 1), ("sum", 1)],
            [(fun.name, fun.cyclomatic_complexity) for fun in functions],
        )
