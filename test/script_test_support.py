import os
import shutil
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path


def bash():
    command = shutil.which("bash")
    if command is None:
        raise unittest.SkipTest("bash is required to run quality scripts")
    return command


def git(work, *args):
    subprocess.run(
        ["git", *args],
        cwd=str(work),
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def git_output(work, *args):
    completed = subprocess.run(
        ["git", *args],
        cwd=str(work),
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return completed.stdout


def write(path, content):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def make_executable(path, content):
    write(path, content)
    path.chmod(path.stat().st_mode | stat.S_IEXEC)


def init_git_repo(work):
    git(work, "init", "-q")
    git(work, "config", "user.email", "test@example.com")
    git(work, "config", "user.name", "Test User")


class TemporaryGitRepository:
    def __enter__(self):
        self._directory = tempfile.TemporaryDirectory()
        self.path = Path(self._directory.name)
        init_git_repo(self.path)
        return self.path

    def __exit__(self, exc_type, exc, traceback):
        self._directory.cleanup()
        return False


def isolated_path_env(fake_bin):
    env = os.environ.copy()
    env["PATH"] = str(fake_bin) + os.pathsep + env["PATH"]
    return env
