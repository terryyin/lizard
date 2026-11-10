"""Missing parts and converter failures stay unpublished for case evidence."""

import json
import shutil
import sys

import pytest

from test.ci_proof_pipeline_helpers import (
    SCRIPTS,
    case_evidence,
    convert_junit,
    fragment,
    run,
    summarize,
    write_junit,
)

pytestmark = pytest.mark.skipif(
    shutil.which("node") is None, reason="CI proof collector needs Node.js"
)


def test_missing_website_fragment_keeps_declared_case_part_missing(tmp_path):
    junit = tmp_path / "junit.xml"
    write_junit(junit)
    outcomes = tmp_path / "outcomes.tsv"
    times = tmp_path / "times.tsv"
    convert_junit(junit, outcomes, times)
    fragments = tmp_path / "fragments"
    assert fragment(outcomes, times, "test", fragments).returncode == 0

    summary_path = tmp_path / "summary.json"
    result = summarize(
        fragments,
        summary_path,
        ["test=1:case", "website/frontend/features=1:case"],
    )
    assert result.returncode == 0
    website = case_evidence(
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
    convert = run([sys.executable, str(SCRIPTS / "junit-outcomes.py"), str(bad)])
    assert convert.returncode != 0
    assert list(fragments.iterdir()) == []
