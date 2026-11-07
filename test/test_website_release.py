"""Exercise website tag/source/retry contracts through the release CLI."""

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "website_release.py"
pytestmark = pytest.mark.skipif(shutil.which("git") is None,
                                reason="git is not installed")


class Checkout:
    def __init__(self, path):
        self.path = path
        self.evidence = path / "evidence"
        self.git("init", "-q", "-b", "master")
        self.sha = self.commit("original")

    def git(self, *args):
        env = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
                   GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t")
        return subprocess.run(("git",) + args, cwd=self.path, env=env,
                              check=True, stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE, universal_newlines=True).stdout.strip()

    def commit(self, text):
        (self.path / "source").write_text(text)
        self.git("add", "source")
        self.git("commit", "-qm", text)
        return self.git("rev-parse", "HEAD")

    def run(self, stage, *args, env=None):
        if env is None and stage in ("prepare", "promote", "phase", "summary"):
            env = dict(os.environ, PATH=str(self.path / "no-cloud-tools"))
        return subprocess.run((sys.executable, str(SCRIPT), stage, "--evidence",
                               str(self.evidence)) + args, cwd=self.path,
                              env=env, stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE, universal_newlines=True)

    def validate(self, tag="web-1.2.3", event=None):
        return self.run("validate", "--tag", tag, "--event-sha", event or self.sha)

    def state(self):
        return json.loads((self.evidence / "release.json").read_text())

    def cloud_state(self, version=None, split=None):
        split = split if split is not None else {"allocations": {"previous": 1}}
        (self.evidence / "service-before.json").write_text(json.dumps({"split": split}))
        (self.evidence / "existing-version.json").write_text(json.dumps(version))


@pytest.fixture
def checkout(tmp_path):
    return Checkout(tmp_path)


@pytest.mark.parametrize("tag", ["web-0.0.0", "web-12.345.6789", "web-" + "1" * 24 + ".0.0"])
def test_valid_tag_maps_to_version_and_exact_source(checkout, tag):
    assert checkout.validate(tag).returncode == 0
    state = checkout.state()
    assert state["version"] == tag.replace(".", "-")
    assert state["source_sha"] == state["checkout_sha"] == checkout.sha


@pytest.mark.parametrize("tag", [
    "1.2.3", "web-1.2", "web-01.2.3", "web-1.02.3", "web-1.2.00",
    "web-1.2.3-rc.1", "web-1.2.3+build", "web--1.2.3", "web-1.2.3\n",
    "web-1.2.3;touch injected", "web-" + "1" * 25 + ".0.0",
])
def test_malformed_or_overlong_tag_fails_without_git_or_cloud(tmp_path, tag):
    result = subprocess.run((sys.executable, str(SCRIPT), "validate", "--tag", tag,
                             "--evidence", str(tmp_path)), cwd=tmp_path,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                            universal_newlines=True)
    assert result.returncode == 2
    assert "Expected web-" in result.stderr or "exceeds 32" in result.stderr
    assert not (tmp_path / "injected").exists()
    assert "source_sha" not in json.loads((tmp_path / "release.json").read_text())


def test_annotated_event_is_peeled_without_resolving_a_moved_tag(checkout):
    checkout.git("tag", "-am", "first", "web-1.2.3")
    event = checkout.git("rev-parse", "web-1.2.3")
    newer = checkout.commit("newer")
    checkout.git("tag", "-fam", "moved", "web-1.2.3")
    checkout.git("checkout", "-q", "--detach", checkout.sha)
    assert checkout.validate(event=event).returncode == 0
    assert checkout.state()["event_sha"] == event
    assert checkout.state()["source_sha"] == checkout.sha
    assert checkout.git("rev-parse", "web-1.2.3^{commit}") == newer


def test_different_checkout_is_not_substituted_for_event(checkout):
    checkout.commit("different")
    result = checkout.validate()
    assert result.returncode == 2
    assert "Checkout does not match the event commit" in result.stderr


def test_unavailable_original_event_does_not_fall_back_to_tag(checkout):
    result = checkout.validate(event="f" * 40)
    assert result.returncode == 2
    assert "original event object is unavailable" in result.stderr


def existing(checkout, **changes):
    version = {"id": "web-1-2-3", "envVariables": {
        "LIZARD_WEBSITE_SOURCE_SHA": checkout.sha}}
    version.update(changes)
    return version


def test_new_version_can_deploy_and_preserves_entire_saved_split(checkout):
    assert checkout.validate().returncode == 0
    split = {"allocations": {"a": 0.4, "b": 0.6}, "shardBy": "COOKIE"}
    checkout.cloud_state(split=split)
    assert checkout.run("prepare").returncode == 0
    assert checkout.state()["action"] == "deploy"
    assert checkout.state()["prior_split"] == split


@pytest.mark.parametrize("share,action", [(0, "deploy"), (0.2, "verify"), (1, "verify")])
def test_matching_identity_selects_safe_retry_alternative(checkout, share, action):
    assert checkout.validate().returncode == 0
    checkout.cloud_state(existing(checkout), {"allocations": {
        "web-1-2-3": share, "previous": 1 - share}})
    assert checkout.run("prepare").returncode == 0
    assert checkout.state()["action"] == action


@pytest.mark.parametrize("share", [0, 1])
@pytest.mark.parametrize("version", [
    {"id": "web-1-2-3"},
    {"id": "web-1-2-3", "envVariables": {"LIZARD_WEBSITE_SOURCE_SHA": "a" * 40}},
])
def test_ambiguous_or_moved_source_cannot_be_redeployed(checkout, share, version):
    assert checkout.validate().returncode == 0
    checkout.cloud_state(version, {"allocations": {"web-1-2-3": share,
                                                  "previous": 1 - share}})
    result = checkout.run("prepare")
    assert result.returncode == 2
    assert "never move the tag or overwrite it" in result.stderr
    assert "action" not in checkout.state()
    assert checkout.run("deploy", env=dict(os.environ, PATH=str(checkout.path / "no-cloud-tools"))).returncode == 2


@pytest.mark.parametrize("service", [
    {}, {"split": None}, {"split": {}}, {"split": {"allocations": None}},
    {"split": {"allocations": {"web-1-2-3": "unknown"}}},
    {"split": {"allocations": {"web-1-2-3": -1}}},
])
def test_unknown_serving_state_fails_with_inspection_guidance(checkout, service):
    assert checkout.validate().returncode == 0
    checkout.cloud_state(existing(checkout))
    (checkout.evidence / "service-before.json").write_text(json.dumps(service))
    result = checkout.run("prepare")
    assert result.returncode == 2
    assert "inspect the service before retrying" in result.stderr
    assert "action" not in checkout.state()


def test_summary_retains_identity_when_authentication_never_happened(checkout):
    assert checkout.validate().returncode == 0
    result = checkout.run("summary", env=dict(os.environ, RELEASE_JOB_RESULT="failure"))
    assert result.returncode == 0
    assert "Result: failure" in result.stdout
    assert checkout.sha in result.stdout
    assert 'current_split: `"unknown"`' in result.stdout
    assert "No prior split was captured" in result.stdout
