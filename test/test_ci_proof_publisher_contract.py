"""Discriminate old collector, omitted/untyped units, and workflow case wiring."""

import json
import shutil
import subprocess
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts" / "ci-proof"
WORKFLOW = ROOT / ".github" / "workflows" / "python-app.yml"
SUMMARY = SCRIPTS / "ci-proof-summary.mjs"
BASE = "e057eb2d31decb4c7e895d39bd126e49190f77b0"
OLD_COLLECTOR_MODULES = (
    "ci-proof-summary.mjs",
    "ci-proof-collect.mjs",
    "ci-proof-contract.mjs",
    "ci-proof-figures.mjs",
    "ci-proof-fragments.mjs",
)
NODE = shutil.which("node")
pytestmark = pytest.mark.skipif(NODE is None, reason="CI proof collector needs Node.js")


def _run(command, **kwargs):
    return subprocess.run(command, capture_output=True, text=True, **kwargs)


def test_old_collector_rejects_unit_flag(tmp_path):
    old = tmp_path / "old-collector"
    old.mkdir()
    for name in OLD_COLLECTOR_MODULES:
        text = _run(
            ["git", "show", f"{BASE}:scripts/ci-proof/{name}"],
            check=True,
            cwd=ROOT,
        ).stdout
        (old / name).write_text(text, encoding="utf-8")
    outcomes = tmp_path / "outcomes.tsv"
    outcomes.write_text("passed\ttest/a.py\n", encoding="utf-8")
    result = _run(
        [
            NODE,
            str(old / "ci-proof-summary.mjs"),
            "fragment",
            "--outcomes",
            str(outcomes),
            "--unit",
            "case",
            "--entry",
            "test",
            "--part",
            "1",
            "--of",
            "1",
            "--attempt",
            "1",
            "--revision",
            "abc",
            "--out",
            str(tmp_path / "frags"),
        ]
    )
    assert result.returncode != 0
    assert "Unknown option '--unit'" in result.stderr


def test_omitted_unit_and_untyped_expect_are_not_case_declarations(tmp_path):
    outcomes = tmp_path / "outcomes.tsv"
    outcomes.write_text("passed\ttest/a.py\n", encoding="utf-8")
    legacy_frags = tmp_path / "legacy-frags"
    omitted = _run(
        [
            NODE,
            str(SUMMARY),
            "fragment",
            "--outcomes",
            str(outcomes),
            "--entry",
            "test",
            "--part",
            "1",
            "--of",
            "1",
            "--attempt",
            "1",
            "--revision",
            "deadbeefcafef00d",
            "--out",
            str(legacy_frags),
        ]
    )
    assert omitted.returncode == 0
    legacy_fragment = json.loads(
        next(legacy_frags.glob("*.json")).read_text(encoding="utf-8")
    )
    assert legacy_fragment["unit"] == "legacy"

    typed = _run(
        [
            NODE,
            str(SUMMARY),
            "summarize",
            "--fragments",
            str(legacy_frags),
            "--expect",
            "test=1:case",
            "--repository",
            "terryyin/lizard",
            "--run-id",
            "42",
            "--attempt",
            "2",
            "--revision",
            "deadbeefcafef00d",
            "--out",
            str(tmp_path / "typed.json"),
        ]
    )
    assert typed.returncode == 0
    typed_summary = json.loads((tmp_path / "typed.json").read_text(encoding="utf-8"))
    evidence = typed_summary["entries"][0]["evidence"][0]
    assert evidence["unit"] == "case"
    assert evidence["parts"] == [{"part": 1, "status": "missing"}]

    case_frags = tmp_path / "case-frags"
    empty_times = tmp_path / "empty-times.tsv"
    empty_times.write_text("", encoding="utf-8")
    assert (
        _run(
            [
                NODE,
                str(SUMMARY),
                "fragment",
                "--outcomes",
                str(outcomes),
                "--times",
                str(empty_times),
                "--unit",
                "case",
                "--entry",
                "test",
                "--part",
                "1",
                "--of",
                "1",
                "--attempt",
                "2",
                "--revision",
                "deadbeefcafef00d",
                "--out",
                str(case_frags),
            ]
        ).returncode
        == 0
    )
    untyped = _run(
        [
            NODE,
            str(SUMMARY),
            "summarize",
            "--fragments",
            str(case_frags),
            "--expect",
            "test=1",
            "--expect",
            "website/frontend/features=1",
            "--repository",
            "terryyin/lizard",
            "--run-id",
            "42",
            "--attempt",
            "2",
            "--revision",
            "deadbeefcafef00d",
            "--out",
            str(tmp_path / "untyped.json"),
        ]
    )
    assert untyped.returncode == 0
    untyped_summary = json.loads(
        (tmp_path / "untyped.json").read_text(encoding="utf-8")
    )
    website = next(
        item
        for item in untyped_summary["entries"]
        if item["path"] == "website/frontend/features"
    )
    assert website["evidence"] == [
        {
            "unit": "legacy",
            "label": None,
            "passed": 0,
            "failed": 0,
            "other": 0,
            "parts": [{"part": 1, "status": "missing"}],
        }
    ]


def test_workflow_wires_case_unit_on_both_producers_and_collector():
    text = WORKFLOW.read_text(encoding="utf-8")
    assert text.count("--unit case") == 2
    assert "--expect test=1:case --expect website/frontend/features=1:case" in text
    assert "matrix.os == 'ubuntu-24.04' && matrix.python-version == '3.13'" in text
    assert "needs.changes.outputs.web == 'true'" in text
    assert "ci-proof-summary-attempt-${{ github.run_attempt }}" in text
    assert "retention-days: 30" in text
    assert "--expect test=1 --expect website/frontend/features=1" not in text
    assert text.count("strategy:") == 1
