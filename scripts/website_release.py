"""Release website candidates from immutable website tags."""

import argparse
import json
import os
import re
import subprocess
import sys
from pathlib import Path

from website_smoke import verify

PROJECT = "lizardanalyzer"
SERVICE = "default"
PUBLIC_URL = "https://www.lizard.ws/"
SOURCE_KEY = "LIZARD_WEBSITE_SOURCE_SHA"
TAG_KEY = "LIZARD_WEBSITE_TAG"
TAG = re.compile(r"web-(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)")


def command(*args):
    return subprocess.run(args, check=True, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, universal_newlines=True).stdout.strip()


def cloud(*args):
    return command("gcloud", *args, "--project=" + PROJECT)


def read(path):
    return json.loads(path.read_text())


def write(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def validate(state, args):
    if not TAG.fullmatch(args.tag):
        raise ValueError("Expected web-<major>.<minor>.<patch>, without leading zeroes or suffixes")
    version = args.tag.replace(".", "-")
    if len(version) > 32:
        raise ValueError("Mapped App Engine version exceeds 32 characters")
    if not re.fullmatch(r"[0-9a-fA-F]{40}", args.event_sha):
        raise ValueError("The immutable event SHA must be a full Git object ID")
    try:
        source = command("git", "rev-parse", "--verify", args.event_sha + "^{commit}")
    except subprocess.CalledProcessError:
        raise ValueError("The original event object is unavailable; recover that exact object before retrying, never resolve a moved tag")
    checkout = command("git", "rev-parse", "HEAD")
    if checkout != source:
        raise ValueError("Checkout does not match the event commit; do not resolve a moved tag")
    state.update(tag=args.tag, version=version, event_sha=args.event_sha,
                 source_sha=source, checkout_sha=checkout, public_url=PUBLIC_URL,
                 result="validated; awaiting preview")
    if os.environ.get("GITHUB_OUTPUT"):
        with open(os.environ["GITHUB_OUTPUT"], "a") as output:
            output.write("source_sha=" + source + "\n")


def inspect(state, args):
    if command("git", "rev-parse", "HEAD") != state["source_sha"]:
        raise ValueError("Tested checkout does not match the validated source")
    write(args.evidence / "service-before.json", json.loads(cloud(
        "app", "services", "describe", SERVICE, "--format=json")))
    ids = cloud("app", "versions", "list", "--service=" + SERVICE,
                "--format=value(id)").splitlines()
    existing = json.loads(cloud("app", "versions", "describe", state["version"],
                               "--service=" + SERVICE, "--format=json")) if state["version"] in ids else None
    write(args.evidence / "existing-version.json", existing)


def prepare(state, args):
    state.pop("action", None)
    split = read(args.evidence / "service-before.json").get("split")
    allocations = split.get("allocations") if isinstance(split, dict) else None
    if not isinstance(allocations, dict) or any(
            isinstance(value, bool) or not isinstance(value, (int, float))
            or not 0 <= value <= 1 for value in allocations.values()):
        raise ValueError("Cannot establish the current traffic split; inspect the service before retrying")
    state["prior_split"] = split
    existing = read(args.evidence / "existing-version.json")
    action = "deploy"
    if existing is not None:
        env = existing.get("envVariables", {}) if isinstance(existing, dict) else {}
        if (not isinstance(existing, dict) or existing.get("id") != state["version"]
                or not isinstance(env, dict) or env.get(SOURCE_KEY) != state["source_sha"]):
            raise ValueError("Existing version identity is unknown or different; never move the tag or overwrite it. Inspect version metadata and publish a new immutable tag")
        if allocations.get(state["version"], 0) > 0:
            action = "verify"
    state["action"] = action
    print("Candidate action:", state["action"])


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
    if version.get("runtime") != "python313" or version.get("envVariables", {}).get(SOURCE_KEY) != state["source_sha"]:
        raise ValueError("Deployed runtime/source identity does not match this release")
    state["preview_url"] = cloud("app", "browse", "--service=" + SERVICE,
                                  "--version=" + state["version"], "--no-launch-browser")


def check_preview(state, args):
    service = json.loads(cloud("app", "services", "describe", SERVICE, "--format=json"))
    write(args.evidence / "service-after.json", service)
    if service["split"] != state["prior_split"]:
        raise ValueError("Traffic changed during a preview-only release; inspect retained service state")
    state["traffic_unchanged"] = True
    write(args.evidence / "analysis.json", verify(state["preview_url"]))
    state["result"] = "preview-only; not promoted"


def summary(state, args):
    result = os.environ.get("RELEASE_JOB_RESULT", "unknown")
    text = ("## Website release\n\nResult: %s\n\n" % (
        state.get("result", "incomplete") if result == "success" else result))
    for key in ("tag", "event_sha", "source_sha", "checkout_sha", "version",
                "stage", "action", "preview_url", "public_url", "prior_split", "error"):
        if key in state:
            text += "- %s: `%s`\n" % (key, json.dumps(state[key]) if isinstance(state[key], dict) else state[key])
    text += "\nThis workflow currently creates verified previews only; public traffic is not promoted.\n"
    if os.environ.get("GITHUB_STEP_SUMMARY"):
        with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as output:
            output.write(text)
    print(text)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("validate", "inspect", "prepare", "deploy", "verify", "summary"))
    parser.add_argument("--tag", default=os.environ.get("RELEASE_TAG", ""))
    parser.add_argument("--event-sha", default=os.environ.get("EVENT_SHA", ""))
    parser.add_argument("--evidence", type=Path, default=Path("release-evidence"))
    args = parser.parse_args(argv)
    args.evidence.mkdir(parents=True, exist_ok=True)
    path = args.evidence / "release.json"
    state = read(path) if path.exists() and args.command != "validate" else {"tag": args.tag}
    if args.command == "summary":
        summary(state, args)
        return 0
    state["stage"] = args.command
    write(path, state)
    try:
        {"validate": validate, "inspect": inspect, "prepare": prepare,
         "deploy": deploy, "verify": check_preview}[args.command](state, args)
        state.pop("error", None)
    except Exception as error:
        state["error"] = str(error) + (": " + error.stderr[-2000:] if isinstance(error, subprocess.CalledProcessError) else "")
        write(path, state)
        print("website_release:", state["error"], file=sys.stderr)
        return 2
    write(path, state)
    return 0


if __name__ == "__main__":
    sys.exit(main())
