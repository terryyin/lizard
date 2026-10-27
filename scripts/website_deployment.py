"""Inspect and publish the tested website with explicit source and traffic guards."""

import json
import subprocess
from pathlib import Path

from website_smoke import verify, verify_redirect
from website_traffic import recovery_command, same_split, traffic_arguments, traffic_split

PROJECT = "lizardanalyzer"
SERVICE = "default"
PUBLIC_URL = "https://www.lizard.ws/"
SOURCE_KEY = "LIZARD_WEBSITE_SOURCE_SHA"
TAG_KEY = "LIZARD_WEBSITE_TAG"


def command(*args):
    return subprocess.run(args, check=True, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, universal_newlines=True).stdout.strip()


def cloud(*args):
    return command("gcloud", *args, "--project=" + PROJECT)


def read(path):
    return json.loads(path.read_text())


def write(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def service_snapshot(args, filename):
    service = json.loads(cloud("app", "services", "describe", SERVICE, "--format=json"))
    write(args.evidence / filename, service)
    return traffic_split(service)


def require_version(state, version):
    if (version.get("id") != state["version"] or version.get("runtime") != "python313"
            or version.get("envVariables", {}).get(SOURCE_KEY) != state["source_sha"]):
        raise ValueError("Deployed version/runtime/source identity does not match this release")


def remember_prior(state, args, split):
    state["prior_split"] = split
    state["recovery_command"] = recovery_command(PROJECT, SERVICE, split)
    write(args.evidence / "release.json", state)


def inspect(state, args):
    if command("git", "rev-parse", "HEAD") != state["source_sha"]:
        raise ValueError("Tested checkout does not match the validated source")
    remember_prior(state, args, service_snapshot(args, "service-before.json"))
    ids = cloud("app", "versions", "list", "--service=" + SERVICE,
                "--format=value(id)").splitlines()
    existing = json.loads(cloud("app", "versions", "describe", state["version"],
                               "--service=" + SERVICE, "--format=json")) if state["version"] in ids else None
    write(args.evidence / "existing-version.json", existing)


def prepare(state, args):
    state.pop("action", None)
    remember_prior(state, args, traffic_split(read(args.evidence / "service-before.json")))
    existing = read(args.evidence / "existing-version.json")
    action = "deploy"
    if existing is not None:
        env = existing.get("envVariables", {}) if isinstance(existing, dict) else {}
        if (not isinstance(existing, dict) or existing.get("id") != state["version"]
                or not isinstance(env, dict) or env.get(SOURCE_KEY) != state["source_sha"]):
            raise ValueError("Existing version identity is unknown or different; never move the tag or overwrite it. Inspect version metadata and publish a new immutable tag")
        if state["prior_split"]["allocations"].get(state["version"], 0) > 0:
            action = "verify"
    state["action"] = action
    print("Candidate action:", action)


def deploy(state, args):
    if state.get("action") not in ("deploy", "verify"):
        raise ValueError("No prepared deployment action")
    if state["action"] == "deploy":
        import yaml
        config = yaml.safe_load(Path("app.yaml").read_text())
        config.setdefault("env_variables", {}).update(
            {SOURCE_KEY: state["source_sha"], TAG_KEY: state["tag"]})
        Path("app.yaml").write_text(yaml.safe_dump(config, sort_keys=False))
    (args.evidence / "upload.txt").write_text(cloud("meta", "list-files-for-upload") + "\n")
    if state["action"] == "deploy":
        output = cloud("app", "deploy", "app.yaml", "--version=" + state["version"],
                       "--no-promote", "--no-stop-previous-version", "--quiet", "--format=json")
        (args.evidence / "deployment.json").write_text(output + "\n")
    version = json.loads(cloud("app", "versions", "describe", state["version"],
                               "--service=" + SERVICE, "--format=json"))
    write(args.evidence / "deployed-version.json", version)
    require_version(state, version)
    state["preview_url"] = cloud("app", "browse", "--service=" + SERVICE,
                                  "--version=" + state["version"], "--no-launch-browser")


def check_preview(state, args):
    state.pop("preview_verified", None)
    split = service_snapshot(args, "service-after.json")
    if not same_split(split, state["prior_split"]):
        raise ValueError("Traffic changed before promotion; inspect retained service state")
    state["traffic_unchanged"] = True
    write(args.evidence / "analysis.json", verify(state["preview_url"]))
    state["preview_verified"] = True
    state["result"] = "preview verified; awaiting promotion"


def promote(state, args):
    if state.get("preview_verified") is not True:
        raise ValueError("Promotion requires successful preview verification; traffic was not changed")
    split = service_snapshot(args, "service-pre-promotion.json")
    if not same_split(split, state["prior_split"]):
        raise ValueError("Traffic changed since capture; inspect the service before retrying")
    require_version(state, json.loads(cloud("app", "versions", "describe", state["version"],
                                          "--service=" + SERVICE, "--format=json")))
    state["promotion_attempted"] = split["allocations"] != {state["version"]: 1}
    write(args.evidence / "release.json", state)
    if state["promotion_attempted"]:
        cloud("app", "services", "set-traffic", SERVICE, "--quiet",
              *traffic_arguments({"allocations": {state["version"]: 1}}))
    state["result"] = "traffic assigned; awaiting public verification"


def check_public(state, args):
    split = service_snapshot(args, "service-public.json")
    state["current_split"] = split
    if split["allocations"] != {state["version"]: 1}:
        raise ValueError("Public traffic does not select the expected release version")
    require_version(state, json.loads(cloud("app", "versions", "describe", state["version"],
                                          "--service=" + SERVICE, "--format=json")))
    write(args.evidence / "public-analysis.json", verify(PUBLIC_URL))
    verify_redirect(PUBLIC_URL)
    state["result"] = "published; public HTTPS verified"


def observe(state, args):
    state["current_split"] = "unknown"
    state["current_split"] = service_snapshot(args, "service-current.json")
