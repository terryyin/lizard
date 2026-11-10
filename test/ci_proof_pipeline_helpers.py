"""Shared converter → fragment helpers for CI proof pipeline tests."""

import json
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts" / "ci-proof"
SUMMARY = SCRIPTS / "ci-proof-summary.mjs"
NODE = shutil.which("node")

IDENTITY = {
    "repository": "terryyin/lizard",
    "runId": 42,
    "runAttempt": 2,
    "revision": "deadbeefcafef00d",
}


def run(command, **kwargs):
    return subprocess.run(command, capture_output=True, text=True, **kwargs)


def write_tsv(path, text):
    # Subprocess text mode on Windows can leave CR in captured stdout; the
    # fragment CLI would otherwise publish paths ending in "\r".
    path.write_text(text.replace("\r\n", "\n").replace("\r", "\n"), encoding="utf-8")


def write_junit(path):
    path.write_text(
        """<testsuite>
        <testcase name="passes" file="test/test_example.py" time="0.125" />
        <testcase name="fails" file="test/test_example.py" time="0.250"><failure /></testcase>
        <testcase name="skips" file="test/test_example.py"><skipped /></testcase>
        <testcase name="fast" file="test/test_other.py" time="0.000" />
        </testsuite>""",
        encoding="utf-8",
    )


def write_cucumber(path):
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
                }
            ]
        ),
        encoding="utf-8",
    )


def convert_junit(report, outcomes, times):
    result = run(
        [sys.executable, str(SCRIPTS / "junit-outcomes.py"), str(report), str(times)],
        check=True,
    )
    write_tsv(outcomes, result.stdout)


def convert_cucumber(report, outcomes, times):
    result = run(
        [NODE, str(SCRIPTS / "cucumber-outcomes.mjs"), str(report), str(times)],
        check=True,
    )
    write_tsv(outcomes, result.stdout)


def fragment(outcomes, times, entry, fragments):
    return run(
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


def summarize(fragments, summary, expects):
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
    return run(command)


def case_evidence(summary, path):
    entry = next(item for item in summary["entries"] if item["path"] == path)
    assert len(entry["evidence"]) == 1
    assert entry["evidence"][0]["unit"] == "case"
    return entry["evidence"][0]
