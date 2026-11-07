"""Observe CI proof input conversion through the report converter CLI."""

import subprocess
import sys
from pathlib import Path

import pytest

SCRIPTS = Path(__file__).resolve().parents[1] / "scripts" / "ci-proof"


@pytest.mark.parametrize("include_times", [False, True])
def test_junit_outcomes_keep_failures_and_untimed_checks(tmp_path, include_times):
    report = tmp_path / "junit.xml"
    report.write_text(
        """<testsuite>
        <testcase name="passes" file="test/test_example.py" time="0.125" />
        <testcase name="fails" file="test/test_example.py" time="0.250"><failure /></testcase>
        <testcase name="skips" file="test/test_example.py"><skipped /></testcase>
        <testcase name="fast" file="test/test_other.py" time="0.000" />
        </testsuite>""",
        encoding="utf-8",
    )
    times = tmp_path / "times.tsv"
    command = [sys.executable, str(SCRIPTS / "junit-outcomes.py"), str(report)]
    if include_times:
        command.append(str(times))

    result = subprocess.run(command, check=True, capture_output=True, text=True)

    assert result.stdout.splitlines() == [
        "passed\ttest/test_example.py",
        "failed\ttest/test_example.py",
        "other\ttest/test_example.py",
        "passed\ttest/test_other.py",
    ]
    if include_times:
        assert times.read_text(encoding="utf-8").splitlines() == [
            "0.125\ttest/test_example.py",
            "0.250\ttest/test_example.py",
            "0.000\ttest/test_other.py",
        ]
