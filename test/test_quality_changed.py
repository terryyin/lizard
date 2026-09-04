import subprocess
import sys
import unittest
from pathlib import Path

from test.script_test_support import (
    TemporaryGitRepository,
    bash,
    git,
    isolated_path_env,
    make_executable,
    write,
)


REPO_ROOT = Path(__file__).resolve().parents[1]


def _run_in_temporary_repository(mode, scenario):
    with TemporaryGitRepository() as work:
        _apply_scenario(work, scenario)
        fake_bin = work / "fake-bin"
        fake_bin.mkdir()
        make_executable(
            fake_bin / "pycodestyle",
            "#!/usr/bin/env bash\nprintf 'pycodestyle %s\\n' \"$*\"\n",
        )
        completed = subprocess.run(
            [bash(), str(REPO_ROOT / "scripts" / ("%s_changed.sh" % mode))],
            cwd=str(work),
            env=isolated_path_env(fake_bin),
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
        )
        return completed.stdout


def _apply_scenario(work, scenario):
    if scenario == "working-tree":
        write(work / "lizard.py", "original = True\n")
        write(work / "lizard_ext" / "example.py", "original = True\n")
        write(work / "lizard_languages" / "ignored.py", "original = True\n")
        write(work / ".gitignore", "lizard_languages/ignored.py\n")
        git(work, "add", ".")
        git(work, "commit", "-qm", "initial")

        write(work / "lizard.py", "changed = True\n")
        git(work, "add", "lizard.py")
        write(work / "lizard_ext" / "example.py", "changed = True\n")
        write(work / "lizard_ext" / "new.py", "untracked = True\n")
        write(work / "lizard_languages" / "ignored.py", "ignored changed\n")
        return

    if scenario == "staged":
        write(work / "lizard.py", "original = True\n")
        write(work / "lizard_ext" / "example.py", "original = True\n")
        write(work / "lizard_languages" / "example.py", "original = True\n")
        git(work, "add", ".")
        git(work, "commit", "-qm", "initial")

        write(work / "lizard.py", "unstaged = True\n")
        write(work / "lizard_ext" / "example.py", "staged = True\n")
        write(work / "lizard_languages" / "example.py", "staged = True\n")
        git(work, "add", "lizard_ext/example.py", "lizard_languages/example.py")
        return

    if scenario == "shared-pep8-config":
        write(work / "setup.cfg", "[pycodestyle]\nmax-line-length = 80\n")
        git(work, "add", "setup.cfg")
        git(work, "commit", "-qm", "initial")
        write(work / "setup.cfg", "[pycodestyle]\nmax-line-length = 120\n")
        git(work, "add", "setup.cfg")
        return

    raise AssertionError("Unknown scenario: %s" % scenario)


@unittest.skipIf(sys.platform == "win32", "quality scripts are bash")
class TestQualityChanged(unittest.TestCase):
    def test_format_selects_staged_unstaged_and_untracked_components(self):
        output = _run_in_temporary_repository("format", "working-tree")
        self.assertEqual(
            "pycodestyle lizard.py\npycodestyle lizard_ext\n",
            output,
        )

    def test_lint_selects_only_staged_components(self):
        output = _run_in_temporary_repository("lint", "staged")
        self.assertEqual(
            "pycodestyle lizard_ext\npycodestyle lizard_languages\n",
            output,
        )

    def test_shared_pep8_config_selects_every_affected_component(self):
        output = _run_in_temporary_repository("lint", "shared-pep8-config")
        self.assertEqual(
            "pycodestyle lizard.py\n"
            "pycodestyle lizard_ext\n"
            "pycodestyle lizard_languages\n",
            output,
        )
