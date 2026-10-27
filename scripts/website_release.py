"""Release website candidates from immutable website tags."""

import argparse
import json
import os
import re
import subprocess
import sys
from pathlib import Path

from website_deployment import (PROJECT, PUBLIC_URL, SERVICE, check_preview,
                                check_public, command, deploy, inspect, observe,
                                prepare, promote, read, write)
TAG = re.compile(r"web-(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)")


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


def phase(state, args):
    if args.phase is None:
        raise ValueError("A workflow phase must be supplied")
    state["stage"] = args.phase


def summary(state, args):
    result = os.environ.get("RELEASE_JOB_RESULT", "unknown")
    text = ("## Website release\n\nResult: %s\n\n" % (
        state.get("result", "incomplete") if result == "success" else result))
    text += "Target: `%s / %s`\n\n" % (PROJECT, SERVICE)
    for key in ("tag", "event_sha", "source_sha", "checkout_sha", "version",
                "stage", "action", "preview_url", "public_url", "prior_split", "error",
                "promotion_attempted", "traffic_error"):
        if key in state:
            text += "- %s: `%s`\n" % (key, json.dumps(state[key]) if isinstance(state[key], dict) else state[key])
    text += "- current_split: `%s`\n" % json.dumps(state.get("current_split", "unknown"))
    if state.get("recovery_command"):
        text += "\nTo restore the saved split after the workflow stops:\n\n```sh\n%s\n```\n" % state["recovery_command"]
    else:
        text += "\nNo prior split was captured; no traffic change was attempted.\n"
    if os.environ.get("GITHUB_STEP_SUMMARY"):
        with open(os.environ["GITHUB_STEP_SUMMARY"], "a") as output:
            output.write(text)
    print(text)


def main(argv=None):
    commands = {"validate": validate, "phase": phase, "inspect": inspect, "prepare": prepare,
                "deploy": deploy, "verify": check_preview, "promote": promote,
                "public": check_public, "observe": observe, "summary": summary}
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=commands)
    parser.add_argument("--tag", default=os.environ.get("RELEASE_TAG", ""))
    parser.add_argument("--event-sha", default=os.environ.get("EVENT_SHA", ""))
    parser.add_argument("--evidence", type=Path, default=Path("release-evidence"))
    parser.add_argument("--phase", choices=("tooling", "checks", "authentication", "cloud-setup"))
    args = parser.parse_args(argv)
    args.evidence.mkdir(parents=True, exist_ok=True)
    path = args.evidence / "release.json"
    state = read(path) if path.exists() and args.command != "validate" else {"tag": args.tag}
    operation = commands[args.command]
    if args.command == "summary":
        operation(state, args)
        return 0
    if args.command != "observe":
        state["stage"] = args.command
    write(path, state)
    try:
        operation(state, args)
        if args.command != "observe":
            state.pop("error", None)
        else:
            state.pop("traffic_error", None)
    except Exception as error:
        detail = str(error) + (": " + error.stderr[-2000:] if isinstance(error, subprocess.CalledProcessError) else "")
        state["traffic_error" if args.command == "observe" else "error"] = detail
        write(path, state)
        print("website_release:", detail, file=sys.stderr)
        return 2
    write(path, state)
    return 0


if __name__ == "__main__":
    sys.exit(main())
