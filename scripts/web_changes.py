"""Decide whether a push or pull request changes the website.

CI runs the browser tests only when this prints ``web=true``. Run it in a
Git checkout with full history:

    python scripts/web_changes.py push BEFORE AFTER
    python scripts/web_changes.py pull_request BASE HEAD

An unresolvable comparison exits nonzero instead of deciding.
"""

import argparse
import os
import subprocess
import sys

# The web change boundary: directories end with "/", other entries are files.
WEB_PATHS = (
    "website/",
    "templates/",
    "index.py",
    "app.yaml",
    ".gcloudignore",
    "requirements.txt",
    "dev_requirements.txt",
    "shell.nix",
    "flake.nix",
    "flake.lock",
    ".gitignore",
    "test/test_website.py",
    "scripts/web_changes.py",
    "test/test_web_changes.py",
    ".github/workflows/python-app.yml",
)
EMPTY_TREE = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
NEW_BRANCH = "0" * 40


class ComparisonError(Exception):
    pass


def git(*args, allowed=(0,)):
    result = subprocess.run(("git",) + args, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    if result.returncode not in allowed:
        raise ComparisonError("git %s failed: %s" % (
            " ".join(args), result.stderr.decode(errors="replace").strip()))
    return result


def merge_base(first, second):
    result = git("merge-base", first, second, allowed=(0, 1))
    return result.stdout.decode().strip() or None


def comparison_start(event, base, head, default_branch):
    if event == "pull_request":
        start = merge_base(base, head)
        if start is None:
            raise ComparisonError("%s and %s have no merge base" % (base, head))
        return start
    if base != NEW_BRANCH:
        return base
    default = git("rev-parse", "--verify", "--quiet",
                  "refs/remotes/origin/%s^{commit}" % default_branch,
                  allowed=(0, 1)).stdout.decode().strip()
    return (default and merge_base(head, default)) or EMPTY_TREE


def changed_paths(start, head):
    output = git("diff", "--name-only", "--no-renames", "-z",
                 start, head, "--").stdout
    return [p.decode(errors="surrogateescape")
            for p in output.split(b"\0") if p]


def is_web(path):
    return any(path.startswith(p) if p.endswith("/") else path == p
               for p in WEB_PATHS)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("event", choices=("push", "pull_request"))
    parser.add_argument("base", help="push: before SHA; pull_request: base SHA")
    parser.add_argument("head", help="push: after SHA; pull_request: head SHA")
    parser.add_argument("--default-branch", default="master")
    args = parser.parse_args(argv)
    try:
        start = comparison_start(args.event, args.base, args.head,
                                 args.default_branch)
        matched = [p for p in changed_paths(start, args.head) if is_web(p)]
    except ComparisonError as error:
        print("web_changes: cannot compare: %s" % error, file=sys.stderr)
        return 2
    decision = "web=%s" % ("true" if matched else "false")
    print("Compared %s..%s" % (start, args.head))
    print(decision)
    for path in matched:
        print("  " + path)
    if os.environ.get("GITHUB_OUTPUT"):
        with open(os.environ["GITHUB_OUTPUT"], "a") as output:
            output.write(decision + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
