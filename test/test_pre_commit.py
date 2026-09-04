import os
import subprocess
import sys
import unittest
from pathlib import Path

from test.script_test_support import (
    TemporaryGitRepository,
    bash,
    git,
    git_output,
    make_executable,
    write,
)


REPO_ROOT = Path(__file__).resolve().parents[1]
PRE_COMMIT = REPO_ROOT / "scripts" / "git-hooks" / "pre-commit"


def _run_in_temporary_repository(scenario):
    with TemporaryGitRepository() as work:
        make_executable(
            work / "scripts" / "run.sh",
            "#!/usr/bin/env bash\n"
            "printf 'runner %s\\n' \"$*\"\n"
            "exit \"${LIZARD_TEST_LINT_EXIT:-0}\"\n",
        )

        write(work / "staged.txt", "original staged\n")
        write(work / "unstaged.txt", "original unstaged\n")
        git(work, "add", ".")
        git(work, "commit", "-qm", "initial")

        write(work / "staged.txt", "intended change\n")
        git(work, "add", "staged.txt")
        write(work / "unstaged.txt", "unrelated change\n")
        write(work / "untracked.txt", "untracked change\n")

        before_status = git_output(work, "status", "--short")
        before_cached = git_output(work, "diff", "--cached", "--binary")
        before_worktree = git_output(work, "diff", "--binary")

        env = os.environ.copy()
        if scenario == "success":
            completed = subprocess.run(
                [bash(), str(PRE_COMMIT)],
                cwd=str(work),
                env=env,
                check=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
            )
            _assert_repository_unchanged(
                work, before_status, before_cached, before_worktree
            )
            return "%srepository unchanged\n" % completed.stdout

        if scenario == "lint-failure":
            make_executable(
                work / ".git" / "hooks" / "pre-commit",
                PRE_COMMIT.read_text(encoding="utf-8"),
            )
            before_head = git_output(work, "rev-parse", "HEAD").strip()
            env = env.copy()
            env["LIZARD_TEST_LINT_EXIT"] = "37"
            completed = subprocess.run(
                ["git", "commit", "-qm", "attempted"],
                cwd=str(work),
                env=env,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
            )
            if completed.returncode == 0:
                raise AssertionError("commit was expected to fail")
            if git_output(work, "rev-parse", "HEAD").strip() != before_head:
                raise AssertionError("HEAD moved after a blocked commit")
            _assert_repository_unchanged(
                work, before_status, before_cached, before_worktree
            )
            return "%scommit blocked\n" % completed.stdout

        raise AssertionError("Unknown scenario: %s" % scenario)


def _assert_repository_unchanged(work, status, cached, worktree):
    if git_output(work, "status", "--short") != status:
        raise AssertionError("git status changed")
    if git_output(work, "diff", "--cached", "--binary") != cached:
        raise AssertionError("staged diff changed")
    if git_output(work, "diff", "--binary") != worktree:
        raise AssertionError("worktree diff changed")


@unittest.skipIf(sys.platform == "win32", "quality scripts are bash")
class TestPreCommit(unittest.TestCase):
    def test_success_lints_without_changing_repository_state(self):
        output = _run_in_temporary_repository("success")
        self.assertEqual(
            "runner make lint-changed\nrepository unchanged\n",
            output,
        )

    def test_lint_failure_blocks_commit_without_changing_pending_work(self):
        output = _run_in_temporary_repository("lint-failure")
        self.assertEqual(
            "runner make lint-changed\ncommit blocked\n",
            output,
        )
