"""Read-only PR evidence collection. No approval, checkout, push or merge actions."""

import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys


FIELDS = (
    "metadata",
    "files",
    "comments",
    "reviews",
    "review_comments",
    "threads",
    "checks",
    "statuses",
    "diff",
)


def encoded(value):
    return (json.dumps(value, sort_keys=True, indent=2) + "\n").encode()


def sha(data):
    return hashlib.sha256(data).hexdigest()


def gh(*args):
    result = subprocess.run(
        ["gh", "api", *args], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    return result.stdout.decode("utf-8")


def pages(endpoint, field=None):
    result = json.loads(
        gh("--method", "GET", "--paginate", "--slurp", endpoint + "?per_page=100")
    )
    return [item for page in result for item in (page[field] if field else page)]


def thread_states(repo, number):
    owner, name = repo.split("/")
    query = """query($owner:String!,$name:String!,$number:Int!,$after:String){
      repository(owner:$owner,name:$name){pullRequest(number:$number){
        reviewThreads(first:100,after:$after){
          nodes{id isResolved isOutdated path line}
          pageInfo{hasNextPage endCursor}
        }
      }}
    }"""
    result, cursor = [], None
    while True:
        args = [
            "graphql",
            "-f",
            "query=" + query,
            "-f",
            "owner=" + owner,
            "-f",
            "name=" + name,
            "-F",
            "number=" + str(number),
        ]
        if cursor:
            args += ["-f", "after=" + cursor]
        data = json.loads(gh(*args))
        if data.get("errors"):
            raise ValueError("GraphQL review-thread collection failed")
        connection = data["data"]["repository"]["pullRequest"]["reviewThreads"]
        result.extend(connection["nodes"])
        if not connection["pageInfo"]["hasNextPage"]:
            return result
        cursor = connection["pageInfo"]["endCursor"]


def identity(metadata):
    """Accept REST metadata or the connected GitHub tool's normalized metadata."""

    def revision(key):
        value = metadata.get(key)
        return value.get("sha") if isinstance(value, dict) else metadata[key + "_sha"]

    return {
        "number": metadata["number"],
        "head": revision("head"),
        "base": revision("base"),
        "state": metadata["state"],
        "updated_at": metadata["updated_at"],
        "merged": metadata.get("merged", False),
    }


def capture(repo, number):
    prefix = "repos/" + repo
    endpoint = prefix + "/pulls/" + str(number)
    metadata = json.loads(gh("--method", "GET", endpoint))
    head = identity(metadata)["head"]
    payload = {
        "metadata": metadata,
        "files": pages(endpoint + "/files"),
        "comments": pages(prefix + "/issues/" + str(number) + "/comments"),
        "reviews": pages(endpoint + "/reviews"),
        "review_comments": pages(endpoint + "/comments"),
        "threads": thread_states(repo, number),
        "checks": pages(prefix + "/commits/" + head + "/check-runs", "check_runs"),
        "statuses": pages(prefix + "/commits/" + head + "/statuses"),
        "diff": gh(
            "--method", "GET", "-H", "Accept: application/vnd.github.diff", endpoint
        ),
    }
    fresh = json.loads(gh("--method", "GET", endpoint))
    if identity(fresh) != identity(metadata):
        raise ValueError("PR changed during capture; discard this read and refresh")
    if len(payload["files"]) != metadata["changed_files"]:
        raise ValueError(
            "incomplete changed-file list; GitHub may have capped the result"
        )
    return payload


def verify(directory):
    manifest = json.loads((directory / "manifest.json").read_text())
    if set(manifest["files"]) != {"packet.json", "diff.patch"}:
        raise ValueError("unexpected manifest file set")
    for name, expected in manifest["files"].items():
        if sha((directory / name).read_bytes()) != expected:
            raise ValueError("packet integrity failure: " + name)
    return json.loads((directory / "packet.json").read_text())


def summarize(directory):
    data = verify(directory)
    meta = data["metadata"]
    checks = Counter(
        x.get("conclusion") or x.get("status", "unknown") for x in data["checks"]
    )
    return {
        "packet": str(directory.resolve()),
        "repo": data["repo"],
        "title": meta.get("title"),
        **identity(meta),
        "counts": {
            key: len(data[key]) for key in FIELDS if key not in ("metadata", "diff")
        },
        "checks": dict(checks),
        "unresolved_threads": sum(not t["isResolved"] for t in data["threads"]),
        "first_paths": [x["filename"] for x in data["files"][:20]],
        "merge_clearance": False,
    }


def pack(payload, output, repo):
    missing = set(FIELDS) - set(payload)
    if missing:
        raise ValueError("incomplete packet: " + ", ".join(sorted(missing)))
    for field in FIELDS[1:-1]:
        if not isinstance(payload[field], list):
            raise ValueError(field + " must contain every page as one array")
    stamp = identity(payload["metadata"])
    if not all(re.fullmatch(r"[0-9a-f]{40}", stamp[key]) for key in ("head", "base")):
        raise ValueError("expected full immutable head and base SHAs")
    if not isinstance(payload["diff"], str):
        raise ValueError("diff must be literal text, not a display-normalized patch")
    packet = {key: payload[key] for key in FIELDS if key != "diff"}
    packet.update(schemaVersion=1, repo=repo)
    packet_bytes, diff_bytes = encoded(packet), payload["diff"].encode("utf-8")
    digest = sha(packet_bytes + diff_bytes)
    directory = (
        output / (repo.replace("/", "--") + "--pr" + str(stamp["number"])) / digest
    )
    if directory.exists():
        verify(directory)
    else:
        directory.mkdir(parents=True)
        (directory / "packet.json").write_bytes(packet_bytes)
        (directory / "diff.patch").write_bytes(diff_bytes)
        (directory / "manifest.json").write_bytes(
            encoded(
                {
                    "schemaVersion": 1,
                    "capturedAt": datetime.now(timezone.utc).isoformat(),
                    "files": {
                        "packet.json": sha(packet_bytes),
                        "diff.patch": sha(diff_bytes),
                    },
                }
            )
        )
    return summarize(directory)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    collect = sub.add_parser(
        "capture", help="collect all pages through read-only gh API calls"
    )
    imported = sub.add_parser(
        "pack",
        help="pack an existing JSON export; required keys: "
        + ", ".join(FIELDS)
        + "; arrays must include every page",
    )
    for command in (collect, imported):
        command.add_argument("--repo", required=True)
        command.add_argument("--out", required=True, type=Path)
    collect.add_argument("--pr", required=True, type=int)
    imported.add_argument("--input", required=True, type=Path)
    inspect = sub.add_parser(
        "inspect", help="verify retained bytes and print a compact summary"
    )
    inspect.add_argument("directory", type=Path)
    args = parser.parse_args()
    try:
        if args.command == "inspect":
            result = summarize(args.directory)
        else:
            if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", args.repo):
                raise ValueError("repo must be owner/name")
            if args.command == "capture":
                if args.pr <= 0:
                    raise ValueError("PR number must be positive")
                payload = capture(args.repo, args.pr)
            else:
                payload = json.loads(args.input.read_text())
            result = pack(payload, args.out, args.repo)
        print(json.dumps(result, sort_keys=True))
        return 0
    except (ValueError, KeyError, OSError, subprocess.CalledProcessError) as error:
        print(json.dumps({"ok": False, "error": str(error)}))
        return 2


if __name__ == "__main__":
    sys.exit(main())
