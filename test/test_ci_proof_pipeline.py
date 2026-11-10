"""Observe converter → fragment → summarize case publication through the copied CLI."""

import json
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts" / "ci-proof"
SUMMARY = SCRIPTS / "ci-proof-summary.mjs"
NODE = shutil.which("node")
pytestmark = pytest.mark.skipif(NODE is None, reason="CI proof collector needs Node.js")

IDENTITY = {
    "repository": "terryyin/lizard",
    "runId": 42,
    "runAttempt": 2,
    "revision": "deadbeefcafef00d",
}


def _run(command, **kwargs):
    return subprocess.run(command, capture_output=True, text=True, **kwargs)


def _write_junit(path):
    path.write_text(
        """<testsuite>
        <testcase name="passes" file="test/test_example.py" time="0.125" />
        <testcase name="fails" file="test/test_example.py" time="0.250"><failure /></testcase>
        <testcase name="skips" file="test/test_example.py"><skipped /></testcase>
        <testcase name="fast" file="test/test_other.py" time="0.000" />
        </testsuite>""",
        encoding="utf-8",
    )


def _write_cucumber(path):
    path.write_text(
        json.dumps(
            [
                {
                    "uri": "features/shared.feature",
                    "elements": [
                        {
                            "type": "background",
                            "steps": [
                                {
                                    "result": {
                                        "status": "passed",
                                        "duration": 9_000_000_000,
                                    }
                                }
                            ],
                        },
                        {
                            "type": "scenario",
                            "before": [
                                {
                                    "result": {
                                        "status": "passed",
                                        "duration": 125_000_000,
                                    }
                                }
                            ],
                            "steps": [
                                {
                                    "result": {
                                        "status": "passed",
                                        "duration": 250_000_000,
                                    }
                                },
                                {"result": {"status": "passed"}},
                            ],
                            "after": [
                                {
                                    "result": {
                                        "status": "passed",
                                        "duration": 500_000_000,
                                    }
                                }
                            ],
                        },
                        {
                            "type": "scenario",
                            "steps": [
                                {
                                    "result": {
                                        "status": "failed",
                                        "duration": 125_000_000,
                                    }
                                }
                            ],
                        },
                        {
                            "type": "scenario",
                            "steps": [{"result": {"status": "skipped"}}],
                        },
                    ],
                },
            ]
        ),
        encoding="utf-8",
    )


def _convert_junit(report, outcomes, times):
    result = _run(
        [sys.executable, str(SCRIPTS / "junit-outcomes.py"), str(report), str(times)],
        check=True,
    )
    outcomes.write_text(result.stdout, encoding="utf-8")


def _convert_cucumber(report, outcomes, times):
    result = _run(
        [NODE, str(SCRIPTS / "cucumber-outcomes.mjs"), str(report), str(times)],
        check=True,
    )
    outcomes.write_text(result.stdout, encoding="utf-8")


def _fragment(outcomes, times, entry, fragments):
    return _run(
        [
            NODE,
            str(SUMMARY),
            "fragment",
            "--outcomes",
            str(outcomes),
            "--times",
            str(times),
            "--by-file",
            "--unit",
            "case",
            "--entry",
            entry,
            "--part",
            "1",
            "--of",
            "1",
            "--attempt",
            str(IDENTITY["runAttempt"]),
            "--revision",
            IDENTITY["revision"],
            "--out",
            str(fragments),
        ]
    )


def _summarize(fragments, summary, expects):
    command = [
        NODE,
        str(SUMMARY),
        "summarize",
        "--fragments",
        str(fragments),
        "--repository",
        IDENTITY["repository"],
        "--run-id",
        str(IDENTITY["runId"]),
        "--attempt",
        str(IDENTITY["runAttempt"]),
        "--revision",
        IDENTITY["revision"],
        "--out",
        str(summary),
    ]
    for expect in expects:
        command.extend(["--expect", expect])
    return _run(command)


def _case_evidence(summary, path):
    entry = next(item for item in summary["entries"] if item["path"] == path)
    assert len(entry["evidence"]) == 1
    assert entry["evidence"][0]["unit"] == "case"
    return entry["evidence"][0]


def test_pipeline_publishes_explicit_case_evidence_for_both_producers(tmp_path):
    junit = tmp_path / "junit.xml"
    cucumber = tmp_path / "cucumber.json"
    _write_junit(junit)
    _write_cucumber(cucumber)

    analyzer_outcomes = tmp_path / "analyzer-outcomes.tsv"
    analyzer_times = tmp_path / "analyzer-times.tsv"
    website_outcomes = tmp_path / "website-outcomes.tsv"
    website_times = tmp_path / "website-times.tsv"
    _convert_junit(junit, analyzer_outcomes, analyzer_times)
    _convert_cucumber(cucumber, website_outcomes, website_times)

    fragments = tmp_path / "fragments"
    assert (
        _fragment(analyzer_outcomes, analyzer_times, "test", fragments).returncode == 0
    )
    assert (
        _fragment(
            website_outcomes,
            website_times,
            "website/frontend/features",
            fragments,
        ).returncode
        == 0
    )

    summary_path = tmp_path / "summary.json"
    result = _summarize(
        fragments,
        summary_path,
        ["test=1:case", "website/frontend/features=1:case"],
    )
    assert result.returncode == 0
    summary = json.loads(summary_path.read_text(encoding="utf-8"))
    assert summary["schemaVersion"] == 2
    assert summary["repository"] == IDENTITY["repository"]
    assert summary["runId"] == IDENTITY["runId"]
    assert summary["runAttempt"] == IDENTITY["runAttempt"]
    assert summary["revision"] == IDENTITY["revision"]
    assert [entry["path"] for entry in summary["entries"]] == [
        "test",
        "website/frontend/features",
    ]

    analyzer = _case_evidence(summary, "test")
    assert (analyzer["passed"], analyzer["failed"], analyzer["other"]) == (2, 1, 1)
    assert analyzer["durationMs"] == 375
    assert analyzer["untimed"] == 1
    assert [file["path"] for file in analyzer["files"]] == [
        "test/test_example.py",
        "test/test_other.py",
    ]
    assert analyzer["parts"][0]["status"] == "present"
    assert analyzer["passed"] + analyzer["failed"] + analyzer["other"] == 4
    assert len(analyzer["files"]) == 2

    website = _case_evidence(summary, "website/frontend/features")
    assert (website["passed"], website["failed"], website["other"]) == (1, 1, 1)
    assert website["durationMs"] == 1000
    assert website["untimed"] == 1
    assert [file["path"] for file in website["files"]] == [
        "website/frontend/features/shared.feature",
    ]
    assert website["parts"][0]["status"] == "present"


def test_missing_website_fragment_keeps_declared_case_part_missing(tmp_path):
    junit = tmp_path / "junit.xml"
    _write_junit(junit)
    outcomes = tmp_path / "outcomes.tsv"
    times = tmp_path / "times.tsv"
    _convert_junit(junit, outcomes, times)
    fragments = tmp_path / "fragments"
    assert _fragment(outcomes, times, "test", fragments).returncode == 0

    summary_path = tmp_path / "summary.json"
    result = _summarize(
        fragments,
        summary_path,
        ["test=1:case", "website/frontend/features=1:case"],
    )
    assert result.returncode == 0
    website = _case_evidence(
        json.loads(summary_path.read_text(encoding="utf-8")),
        "website/frontend/features",
    )
    assert (website["passed"], website["failed"], website["other"]) == (0, 0, 0)
    assert website["parts"] == [{"part": 1, "status": "missing"}]


def test_converter_failure_creates_no_valid_fragment(tmp_path):
    bad = tmp_path / "bad.xml"
    bad.write_text("<not-xml", encoding="utf-8")
    fragments = tmp_path / "fragments"
    fragments.mkdir()
    convert = _run([sys.executable, str(SCRIPTS / "junit-outcomes.py"), str(bad)])
    assert convert.returncode != 0
    assert list(fragments.iterdir()) == []
