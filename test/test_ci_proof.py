"""Observe CI proof input conversion through the report converter CLI."""

import json
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPTS = Path(__file__).resolve().parents[1] / "scripts" / "ci-proof"
NODE = shutil.which("node")


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


@pytest.mark.skipif(NODE is None, reason="Cucumber proof conversion needs Node.js")
@pytest.mark.parametrize("include_times", [False, True])
def test_cucumber_outcomes_keep_failures_and_untimed_scenarios(tmp_path, include_times):
    report = tmp_path / "cucumber.json"
    report.write_text(json.dumps([
        {
            "uri": "features/shared.feature",
            "elements": [
                {
                    "type": "background",
                    "steps": [{"result": {"status": "passed", "duration": 9_000_000_000}}],
                },
                {
                    "type": "scenario",
                    "before": [{"result": {"status": "passed", "duration": 125_000_000}}],
                    "steps": [
                        {"result": {"status": "passed", "duration": 250_000_000}},
                        {"result": {"status": "passed"}},
                    ],
                    "after": [{"result": {"status": "passed", "duration": 500_000_000}}],
                },
                {
                    "type": "scenario",
                    "steps": [{"result": {"status": "failed", "duration": 125_000_000}}],
                },
                {"type": "scenario", "steps": [{"result": {"status": "skipped"}}]},
            ],
        },
        {
            "uri": "features/zero.feature",
            "elements": [{
                "type": "scenario",
                "steps": [{"result": {"status": "passed", "duration": 0}}],
            }],
        },
    ]), encoding="utf-8")
    times = tmp_path / "times.tsv"
    command = [NODE, str(SCRIPTS / "cucumber-outcomes.mjs"), str(report)]
    if include_times:
        command.append(str(times))

    result = subprocess.run(command, check=True, capture_output=True, text=True)

    assert result.stdout.splitlines() == [
        "passed\twebsite/frontend/features/shared.feature",
        "failed\twebsite/frontend/features/shared.feature",
        "other\twebsite/frontend/features/shared.feature",
        "passed\twebsite/frontend/features/zero.feature",
    ]
    if include_times:
        assert times.read_text(encoding="utf-8").splitlines() == [
            "0.875\twebsite/frontend/features/shared.feature",
            "0.125\twebsite/frontend/features/shared.feature",
            "0\twebsite/frontend/features/zero.feature",
        ]
