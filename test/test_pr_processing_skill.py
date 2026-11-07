"""Observable contracts of the PR evidence helpers; no parser implementations mocked."""

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / ".agents/skills/lizard-process-pr/scripts"


def load_helper(name):
    spec = importlib.util.spec_from_file_location(name, SCRIPTS / (name + ".py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def packet_payload():
    return {
        "metadata": {
            "number": 123,
            "title": "An example PR",
            "state": "open",
            "head": {"sha": "a" * 40},
            "base": {"sha": "b" * 40},
            "updated_at": "2026-10-28T00:00:00Z",
        },
        "diff": "diff --git a/a.py b/a.py\n+literal \\n and $() bytes\n",
        "files": [{"filename": "a.py"}],
        "comments": [],
        "reviews": [],
        "review_comments": [],
        "threads": [{"isResolved": False}],
        "checks": [{"status": "in_progress", "conclusion": None}],
        "statuses": [],
    }


def packet_command(*args):
    return subprocess.run(
        [sys.executable, str(SCRIPTS / "pr_packet.py"), *map(str, args)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )


def test_packet_retains_literal_evidence_and_is_idempotent(tmp_path):
    payload = packet_payload()
    source = tmp_path / "export.json"
    source.write_text(json.dumps(payload))
    args = (
        "pack",
        "--input",
        source,
        "--repo",
        "terryyin/lizard",
        "--out",
        tmp_path / "packets",
    )
    first, second = packet_command(*args), packet_command(*args)
    assert first.returncode == second.returncode == 0
    assert first.stdout == second.stdout
    summary = json.loads(first.stdout)
    packet = Path(summary["packet"])
    assert (packet / "diff.patch").read_text() == payload["diff"]
    assert summary["unresolved_threads"] == 1
    assert summary["checks"] == {"in_progress": 1}
    assert summary["merge_clearance"] is False
    assert packet_command("inspect", packet).stdout == first.stdout
    payload["metadata"]["head"]["sha"] = "c" * 40
    source.write_text(json.dumps(payload))
    changed = json.loads(packet_command(*args).stdout)
    assert changed["packet"] != summary["packet"]
    assert packet.exists()


def test_packet_rejects_incomplete_or_tampered_evidence(tmp_path):
    packet = load_helper("pr_packet")
    payload = packet_payload()
    payload.pop("reviews")
    with pytest.raises(ValueError, match="incomplete packet"):
        packet.pack(payload, tmp_path, "terryyin/lizard")
    summary = packet.pack(packet_payload(), tmp_path, "terryyin/lizard")
    directory = Path(summary["packet"])
    (directory / "diff.patch").write_text("corrupted")
    inspected = packet_command("inspect", directory)
    assert inspected.returncode == 2
    assert "integrity failure" in json.loads(inspected.stdout)["error"]


def test_paired_statistics_require_every_workload_to_meet_strict_bound():
    evaluate = load_helper("paired_benchmark").evaluate
    assert evaluate([{"baseline": 2, "candidate": 2} for _ in range(20)])["passes"]
    boundary = evaluate([{"baseline": 2, "candidate": 2.06} for _ in range(20)])
    assert not boundary["passes"]
    assert boundary["paired_95pct_ci"][1] == pytest.approx(3.0)
    noisy = [{"baseline": 2, "candidate": value} for value in [1.6, 2.5] * 10]
    assert not evaluate(noisy)["passes"]
    short = evaluate([{"baseline": 0.2, "candidate": 0.1} for _ in range(20)])
    assert short["verdict"] == "inconclusive-short-batches"
    assert not short["passes"]
    with pytest.raises(ValueError):
        evaluate([{"baseline": 2, "candidate": 2}] * 19)
    with pytest.raises(ValueError):
        evaluate([{"baseline": 2, "candidate": float("nan")}] * 20)


@pytest.mark.skipif(os.name == "nt", reason="paired runner uses POSIX pipe selection")
@pytest.mark.parametrize(
    "workload,extension,filename,source",
    [
        ("api", None, "example.js", "function f(a) { return a ? 1 : 2; }"),
        ("api-parameters", None, "example.ts", "function f(a: number) { return a; }"),
        ("cli", "ns", "example.py", "def f(a):\n    if a:\n        return a\n"),
    ],
)
def test_worker_uses_selected_checkout_and_preserves_observations(
    tmp_path, workload, extension, filename, source
):
    benchmark = load_helper("paired_benchmark")
    corpus = tmp_path / "corpus"
    corpus.mkdir()
    (corpus / filename).write_text(source)
    spec = {
        "checkout": str(ROOT),
        "corpus": str(corpus),
        "workload": workload,
        "extension": extension,
    }
    with (tmp_path / "stderr.log").open("w") as log:
        worker = benchmark.Worker(spec, tmp_path, "probe", log)
        try:
            ready = worker.receive()
            assert ready["ready"] and ready["files"] == 1
            assert ready["modules"]["lizard"] == str(ROOT / "lizard.py")
            assert all(ROOT in Path(path).parents for path in ready["modules"].values())
            assert worker.measure(1) > 0
            assert (
                worker.request(0)["observation_sha256"] == ready["observation_sha256"]
            )
        finally:
            worker.close()


@pytest.mark.skipif(os.name == "nt", reason="paired runner uses POSIX pipe selection")
def test_failed_setup_cannot_pass_or_overwrite_previous_result(tmp_path):
    output = tmp_path / "result.json"
    args = [
        sys.executable,
        str(SCRIPTS / "paired_benchmark.py"),
        "--baseline",
        str(tmp_path / "absent"),
        "--candidate",
        str(ROOT),
        "--corpus",
        str(tmp_path / "corpus"),
        "--workload",
        "api",
        "--out",
        str(output),
    ]
    first = subprocess.run(
        args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
    )
    assert first.returncode == 1
    retained = output.read_bytes()
    assert json.loads(retained)["passes"] is False
    assert "missing lizard.py" in json.loads(retained)["error"]
    second = subprocess.run(
        args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
    )
    assert second.returncode == 2
    assert output.read_bytes() == retained


def test_source_integrity_excludes_evolving_acceptance_records(tmp_path):
    source_state = load_helper("paired_benchmark").source_state
    (tmp_path / "lizard.py").write_text("version = 1\n")
    original = source_state(tmp_path)
    (tmp_path / "PLAN.md").write_text("accepted proof\n")
    assert source_state(tmp_path) == original
    (tmp_path / "lizard.py").write_text("version = 2\n")
    assert source_state(tmp_path) != original


def test_cli_warning_exit_is_allowed_but_a_python_crash_is_not_timed(tmp_path):
    run_cli = load_helper("lizard_measurement").run_cli
    warning = tmp_path / "warning.py"
    warning.write_text("print('threshold warning')\nraise SystemExit(1)\n")
    assert run_cli([sys.executable, str(warning)])["returncode"] == 1
    crash = tmp_path / "crash.py"
    crash.write_text("raise RuntimeError('failed analysis')\n")
    for capture in (True, False):
        with pytest.raises(ValueError, match="failed analysis"):
            run_cli([sys.executable, str(crash)], capture=capture)
