"""Observe publication guards, recovery and failure reports through the CLI."""

import json
import os
import shlex

import pytest

from test.test_website_release import Checkout, pytestmark


@pytest.fixture
def checkout(tmp_path):
    return Checkout(tmp_path)


@pytest.mark.parametrize("mode", ["COOKIE", "RANDOM", "IP"])
def test_saved_multi_version_recovery_preserves_weights_and_sharding(checkout, mode):
    assert checkout.validate().returncode == 0
    split = {"allocations": {"a": 0.4, "b": 0.6}, "shardBy": mode}
    checkout.cloud_state(split=split)
    assert checkout.run("prepare").returncode == 0
    state = checkout.state()
    assert state["prior_split"] == split
    assert shlex.split(state["recovery_command"]) == [
        "gcloud", "app", "services", "set-traffic", "default",
        "--project=lizardanalyzer", "--splits=a=0.4,b=0.6", "--split-by=" + mode.lower()]


def test_omitted_sharding_uses_explicit_gcloud_ip_default(checkout):
    assert checkout.validate().returncode == 0
    checkout.cloud_state()
    assert checkout.run("prepare").returncode == 0
    assert checkout.state()["recovery_command"].endswith("--split-by=ip")


@pytest.mark.parametrize("split", [
    {"allocations": {}}, {"allocations": {"a": 0.4}},
    {"allocations": {"a": 1}, "shardBy": "unknown"},
])
def test_unrecoverable_split_fails_before_a_deploy_action(checkout, split):
    assert checkout.validate().returncode == 0
    checkout.cloud_state(split=split)
    result = checkout.run("prepare")
    assert result.returncode == 2
    assert "inspect the service before retrying" in result.stderr
    assert "action" not in checkout.state()


@pytest.mark.parametrize("preview_verified", [None, False])
def test_failed_or_missing_preview_cannot_reach_cloud_promotion(checkout, preview_verified):
    assert checkout.validate().returncode == 0
    state = checkout.state()
    state["preview_verified"] = preview_verified
    (checkout.evidence / "release.json").write_text(json.dumps(state))
    result = checkout.run("promote")  # PATH deliberately contains no gcloud.
    assert result.returncode == 2
    assert "Promotion requires successful preview verification" in result.stderr
    assert "promotion_attempted" not in checkout.state()


@pytest.mark.parametrize("phase", ["tooling", "checks", "authentication", "cloud-setup"])
def test_external_step_failure_reports_its_actual_phase(checkout, phase):
    assert checkout.validate().returncode == 0
    assert checkout.run("phase", "--phase", phase).returncode == 0
    result = checkout.run("summary", env=dict(os.environ, RELEASE_JOB_RESULT="failure",
                                              PATH=str(checkout.path / "no-cloud-tools")))
    assert result.returncode == 0
    assert "Result: failure" in result.stdout
    assert "stage: `%s`" % phase in result.stdout
    assert "No prior split was captured; no traffic change was attempted" in result.stdout


def test_post_promotion_failure_reports_unknown_traffic_and_exact_recovery(checkout):
    assert checkout.validate().returncode == 0
    checkout.cloud_state(split={"allocations": {"a": 0.4, "b": 0.6}, "shardBy": "COOKIE"})
    assert checkout.run("prepare").returncode == 0
    state = checkout.state()
    state.update(stage="public", promotion_attempted=True, error="public request failed")
    (checkout.evidence / "release.json").write_text(json.dumps(state))
    result = checkout.run("observe", env=dict(os.environ, PATH=str(checkout.path / "no-cloud-tools")))
    assert result.returncode == 2
    assert checkout.state()["stage"] == "public"
    assert checkout.state()["error"] == "public request failed"
    result = checkout.run("summary", env=dict(os.environ, RELEASE_JOB_RESULT="failure",
                                              PATH=str(checkout.path / "no-cloud-tools")))
    assert "Result: failure" in result.stdout
    assert 'current_split: `"unknown"`' in result.stdout
    assert "public request failed" in result.stdout
    assert state["recovery_command"] in result.stdout
    assert "published; public HTTPS verified" not in result.stdout


def test_identity_failure_keeps_the_successfully_captured_prior_split(checkout):
    assert checkout.validate().returncode == 0
    split = {"allocations": {"previous": 1}}
    checkout.cloud_state(version={"id": "web-1-2-3"}, split=split)
    assert checkout.run("prepare").returncode == 2
    state = checkout.state()
    assert state["prior_split"] == split
    result = checkout.run("summary", env=dict(os.environ, RELEASE_JOB_RESULT="failure",
                                              PATH=str(checkout.path / "no-cloud-tools")))
    assert state["recovery_command"] in result.stdout
    assert "No prior split was captured" not in result.stdout
