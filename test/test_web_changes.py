"""Observe the CI web change selector on real Git histories."""

import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "web_changes.py"
NEW_BRANCH = "0" * 40

pytestmark = pytest.mark.skipif(shutil.which("git") is None,
                                reason="git is not installed")


class Repo:
    def __init__(self, path):
        self.path = path
        self.git("init", "-q", "-b", "master")

    def git(self, *args):
        env = dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
                   GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t")
        return subprocess.run(("git",) + args, cwd=self.path, env=env,
                              check=True, stdout=subprocess.PIPE,
                              universal_newlines=True).stdout.strip()

    def commit(self, written=(), deleted=()):
        """Commit writing ``{path: text}`` and deleting ``deleted`` paths."""
        for path in deleted:
            self.git("rm", "-q", path)
        for path, text in dict(written).items():
            target = self.path / path
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(text)
            self.git("add", str(target))
        self.git("commit", "-q", "--allow-empty", "-m", "change")
        return self.git("rev-parse", "HEAD")

    def select(self, event, base, head):
        output = self.path / "github_output"
        output.write_text("")
        env = dict(os.environ, GITHUB_OUTPUT=str(output))
        result = subprocess.run((sys.executable, str(SCRIPT), event, base, head),
                                cwd=self.path, env=env, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, universal_newlines=True)
        return result, output.read_text()


@pytest.fixture
def repo(tmp_path):
    repo = Repo(tmp_path)
    repo.commit({"README": "lizard"})
    return repo


def decide(repo, base, head, event="push"):
    result, output = repo.select(event, base, head)
    assert result.returncode == 0, result.stderr
    return output.strip(), result.stdout


def push_of(repo, written=(), deleted=()):
    before = repo.git("rev-parse", "HEAD")
    return decide(repo, before, repo.commit(written, deleted))


def test_website_change_runs_browser_tests(repo):
    output, log = push_of(repo, {"website/frontend/src/App.jsx": "app"})
    assert output == "web=true"
    assert "website/frontend/src/App.jsx" in log


def test_mixed_change_runs_browser_tests(repo):
    output, _ = push_of(repo, {"index.py": "app", "docs/website.md": "notes"})
    assert output == "web=true"


@pytest.mark.parametrize("unrelated", [
    "docs/website.md",
    "lizard_core/options.py",
    "lizard_languages/python.py",
    ".planning/seeds/SEED-012-website-evolution.md",
])
def test_unrelated_change_skips_browser_tests(repo, unrelated):
    output, log = push_of(repo, {unrelated: "changed"})
    assert output == "web=false"
    assert "web=false" in log


def test_deleting_a_web_path_runs_browser_tests(repo):
    repo.commit({"templates/index.html": "page"})
    output, log = push_of(repo, deleted=["templates/index.html"])
    assert output == "web=true"
    assert "templates/index.html" in log


def test_renaming_out_of_the_web_tree_runs_browser_tests(repo):
    repo.commit({"website/helper.py": "code\n" * 20})
    before = repo.git("rev-parse", "HEAD")
    (repo.path / "lizard_ext").mkdir()
    repo.git("mv", "website/helper.py", "lizard_ext/helper.py")
    output, log = decide(repo, before, repo.commit())
    assert output == "web=true"
    assert "website/helper.py" in log


def test_pull_request_ignores_web_changes_only_on_the_base(repo):
    repo.git("checkout", "-q", "-b", "feature")
    head = repo.commit({"docs/website.md": "notes"})
    repo.git("checkout", "-q", "master")
    base = repo.commit({"website/app.py": "app"})
    assert decide(repo, base, head, "pull_request")[0] == "web=false"


def test_new_branch_compares_with_its_default_branch_merge_base(repo):
    repo.git("checkout", "-q", "-b", "feature")
    docs_only = repo.commit({"docs/website.md": "notes"})
    repo.git("checkout", "-q", "master")
    repo.git("update-ref", "refs/remotes/origin/master",
             repo.commit({"website/app.py": "app"}))
    assert decide(repo, NEW_BRANCH, docs_only)[0] == "web=false"
    repo.git("checkout", "-q", "feature")
    assert decide(repo, NEW_BRANCH, repo.commit({"app.yaml": "x"}))[0] == "web=true"


def test_initial_repository_compares_with_the_empty_tree(tmp_path):
    repo = Repo(tmp_path)
    output, log = decide(repo, NEW_BRANCH, repo.commit({"index.py": "app"}))
    assert output == "web=true"
    assert "4b825dc642cb6eb9a060e54bf8d69288fbee4904" in log


def test_unresolvable_comparison_fails_visibly(repo):
    result, output = repo.select("push", "1" * 40, repo.commit({"README": "x"}))
    assert result.returncode != 0
    assert "cannot compare" in result.stderr
    assert "web=" not in result.stdout + output


def test_workflow_runs_web_job_from_selector_output_and_python_independently():
    workflow = (ROOT / ".github" / "workflows" / "python-app.yml").read_text()
    jobs = dict(re.findall(r"^  (\w+):\n((?:    .*\n|\n)*)", workflow, re.M))
    assert "scripts/web_changes.py" in jobs["changes"]
    step = re.search(r"id: (\w+)", jobs["changes"]).group(1)
    assert "web: ${{ steps.%s.outputs.web }}" % step in jobs["changes"]
    assert "needs: changes" in jobs["web"]
    assert "if: needs.changes.outputs.web == 'true'" in jobs["web"]
    assert "needs:" not in jobs["build"]
