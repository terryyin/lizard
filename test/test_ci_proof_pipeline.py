"""Observe converter → fragment → summarize case publication through the copied CLI."""

import json
import shutil

import pytest

from test.ci_proof_pipeline_helpers import (
    IDENTITY,
    case_evidence,
    convert_cucumber,
    convert_junit,
    fragment,
    summarize,
    write_cucumber,
    write_junit,
)

pytestmark = pytest.mark.skipif(
    shutil.which("node") is None, reason="CI proof collector needs Node.js"
)


def test_pipeline_publishes_explicit_case_evidence_for_both_producers(tmp_path):
    junit = tmp_path / "junit.xml"
    cucumber = tmp_path / "cucumber.json"
    write_junit(junit)
    write_cucumber(cucumber)

    analyzer_outcomes = tmp_path / "analyzer-outcomes.tsv"
    analyzer_times = tmp_path / "analyzer-times.tsv"
    website_outcomes = tmp_path / "website-outcomes.tsv"
    website_times = tmp_path / "website-times.tsv"
    convert_junit(junit, analyzer_outcomes, analyzer_times)
    convert_cucumber(cucumber, website_outcomes, website_times)

    fragments = tmp_path / "fragments"
    assert (
        fragment(analyzer_outcomes, analyzer_times, "test", fragments).returncode == 0
    )
    assert (
        fragment(
            website_outcomes,
            website_times,
            "website/frontend/features",
            fragments,
        ).returncode
        == 0
    )

    summary_path = tmp_path / "summary.json"
    result = summarize(
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

    analyzer = case_evidence(summary, "test")
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

    website = case_evidence(summary, "website/frontend/features")
    assert (website["passed"], website["failed"], website["other"]) == (1, 1, 1)
    assert website["durationMs"] == 1000
    assert website["untimed"] == 1
    assert [file["path"] for file in website["files"]] == [
        "website/frontend/features/shared.feature",
    ]
    assert website["parts"][0]["status"] == "present"
