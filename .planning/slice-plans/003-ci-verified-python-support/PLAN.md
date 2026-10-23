# Plan: Claim support only for the Python versions CI verifies

**Identity:** SEED-006#ci-verified-python-support

**Source:** [Claim support only for the Python versions CI verifies](../../seeds/SEED-006-python-version-support.md#ci-verified-python-support)

## Goal and scope

A developer choosing an interpreter can trust that every Python version
Lizard claims to support, on PyPI and in the README, is one the test suite
runs on in continuous integration.

Included: the `python-app.yml` matrix widened to CPython 3.8, 3.9, 3.10,
3.11, 3.12, 3.13 and 3.14 on `ubuntu-latest` and `windows-latest`, installing
Lizard with `python -m pip install .`; the `setup.py` Python classifiers and
the README's verified-versions sentence naming exactly the versions the matrix
runs; one test in `test/` that fails naming the difference when the three
disagree; `.travis.yml` deleted.

Excluded (the story's deferred promises): `python_requires` metadata; PyPy or
other non-CPython interpreters; upgrading `actions/checkout@v3` and
`actions/setup-python@v3` unless the widened run cannot find an interpreter
otherwise; the release workflow's build interpreter; any analyzer change.

Assumptions: 3.8 stays the floor and 3.14 is the newest released minor at
planning (2026-10-23). 14 matrix jobs per push is acceptable: the suite runs
in about three seconds, so each job is runner setup. Every slice ends in a
committed state where the full suite and `nix develop -c make` are green, as
`AGENTS.md` requires before completing a change. Boundary rule from the
story: an interpreter that fails in CI on Linux or Windows leaves the matrix,
the classifiers and the README together in slice 2, and the failure is queued
as its own story.

## Outside-in proof

| Promise (key example) | Owning slice | Observable proof |
| --- | --- | --- |
| CI runs the suite on the seven versions on both OSes (ex. 2) | 1 | After the slice's push, `gh run list --workflow python-app.yml --branch <story branch> --limit 1` shows the run for the pushed SHA `completed success`, and `gh run view <id> --json jobs --jq '.jobs[] | "\(.name) \(.conclusion)"'` lists 14 `success` lines, one per `(os, version)` pair |
| Lizard installs with pip where `setuptools` is absent (ex. 2) | 1 | Local: fresh `nix shell nixpkgs#python314 -c python -m venv $CLAUDE_JOB_DIR/tmp/venv314`, then `.../bin/python -m pip install .`, then `.../bin/lizard lizard_ext/auto_open.py` prints the total line `17 7.0 2.0 67.5 2 0 0.00 0.00`; in CI, the `build and Install` step of every job succeeds |
| Classifiers and README name exactly the verified set (ex. 1) | 2 | `grep -c 'Programming Language :: Python :: 3\.' setup.py` prints 7; `grep -n 'The test suite runs on Python' README.rst` shows the sentence listing 3.8, 3.9, 3.10, 3.11, 3.12, 3.13 and 3.14; the new test passes |
| A mismatch fails the suite naming the difference (ex. 3) | 2 | Scratch: add `- "3.15"` to the matrix, `nix develop -c python -m pytest test/testPythonVersionClaims.py` fails with a message containing `3.15`; revert; the test passes again. Repeat once with a classifier removed instead |
| No retired CI configuration claims versions (ex. 4) | 2 | `test ! -e .travis.yml`; `grep -rn -i travis --exclude-dir=.git --exclude-dir=.planning --exclude-dir=.claude --exclude-dir=.agents .` prints nothing |
| Analyzer behavior unchanged (story boundary) | 1, 2 | Full suite green at each slice boundary; `nix develop -c make` green |

## Decisive premises

| Premise | Consuming operation | Observation | Result |
| --- | --- | --- | --- |
| Trunk CI is green on the current 3.8/3.10 matrix | Slice 1 starts from a passing workflow | `gh run list --workflow python-app.yml --branch master --limit 3` at `84b2aaa` | Three latest runs `completed success`; settled |
| The suite passes on 3.11, 3.12, 3.13 and 3.14 | Slice 1 sizing: no analyzer fix expected | In this workspace at `84b2aaa`, `nix shell --impure --expr '(builtins.getFlake "nixpkgs").legacyPackages.${builtins.currentSystem}.python3NN.withPackages (ps: with ps; [pytest pygments pathspec jinja2])' -c python -m pytest test -q` for NN in 11, 12, 13, 14 | `1651 passed, 7 skipped` on each (3.14.8, 3.13.16, 3.12.15, 3.11.17); without `jinja2` only the eight HTML-output tests fail, which `dev_requirements.txt` already covers in CI; settled for macOS. Linux and Windows are the remainder, observed by slice 1's CI run under the boundary rule |
| The suite passes on 3.9 | Same | `nix develop -c python -m pytest test -q` (dev shell is Python 3.9.11) | `1651 passed, 7 skipped`; settled |
| A fresh 3.12+ interpreter has no `setuptools`, so `python setup.py build install` cannot be relied on | Slice 1's install step change | Fresh 3.14 venv: `python -c "import setuptools"` | `ModuleNotFoundError: No module named 'setuptools'`; settled |
| `python -m pip install .` installs a working Lizard on such an interpreter | Slice 1's install step and its local proof | Same venv: `python -m pip install .` then `lizard lizard_ext/auto_open.py` | Installed; total line `17 7.0 2.0 67.5 2 0 0.00 0.00`; settled. The install left ignored `build/` and `lizard.egg-info/` in the workspace, removed afterwards |
| The CI observer tolerates a wider matrix | Slice 1's CI observation | Read `scripts/dough-ci.mjs`: it classifies whole runs by `status`/`conclusion` and diagnoses failed jobs by name, with no job-name expectations | Settled |
| Nothing reads `.travis.yml` | Slice 2's deletion | `grep -rn -i travis` over the checkout excluding planning and skill directories | Only `.planning/done` records of the removed badge; settled |
| The three claim locations can be read without a new dependency | Slice 2's test | `python-app.yml` lists versions as `- "3.x"` lines under `python-version:`; `setup.py` classifiers are `'Programming Language :: Python :: 3.x'` literals; the README sentence starts `The test suite runs on Python` and ends at `in continuous integration` | Each is matched by a regular expression on the file text; PyYAML is not in `dev_requirements.txt` and is not needed; settled |

The one premise not fully observed, that 3.9 and 3.11 to 3.14 pass on
`ubuntu-latest` and `windows-latest`, can only be observed by running the
workflow. Slice 1 is that probe: its cheap part is settled above, and its CI
result decides the set slice 2 claims. The story's boundary rule bounds a
failure, so no dependent slice is stopped by it.

## Slices

### 1. CI verifies Python 3.8 through 3.14 on Linux and Windows
Type: Behavior
Status: planned
Proof: local 3.14 venv `pip install .` runs Lizard with the baseline total line; full suite and `nix develop -c make` green; after the push, the `Python application` run for the pushed SHA is `completed success` with 14 `success` jobs. The CI result is an external wait that decomposition cannot reduce; the slice is complete only when it is observed.

Behavior: the matrix lists `"3.8"` and `"3.10"` and the `build and Install`
step runs `python setup.py build install` → the matrix lists `"3.8"`,
`"3.9"`, `"3.10"`, `"3.11"`, `"3.12"`, `"3.13"` and `"3.14"` and the step runs
`python -m pip install .` → a push runs the suite on all 14 version and OS
pairs, every job green. If a job fails for an interpreter the runner cannot
find, upgrading `actions/setup-python` (and `actions/checkout`) within this
slice is the first remedy. If a job fails because the suite fails on that
interpreter, record it under **Learnings**, keep the version in the matrix
no further, and carry the reduced set into slice 2 under the boundary rule;
the failure becomes a separate story, not a fix here.

### 2. PyPI classifiers, the README and the matrix name the same verified versions, and a test keeps them agreeing
Type: Behavior
Status: planned
Proof: `test/testPythonVersionClaims.py` passes; the scratch mismatch (ex. 3) fails naming `3.15`, and a removed classifier fails naming it; `grep -c` on the classifiers prints the matrix size; the README sentence lists the set; `test ! -e .travis.yml`; full suite and `nix develop -c make` green.

Behavior: the classifiers list 3.8 to 3.11, the README says the suite runs
on 3.8 and 3.10, and `.travis.yml` lists 3.8, 3.10 and PyPy → the
classifiers list exactly the versions slice 1 verified, the README sentence
reads "The test suite runs on Python 3.8, 3.9, 3.10, 3.11, 3.12, 3.13 and
3.14, on Linux and Windows, in continuous integration; other versions are not
verified." (with the verified set), a new `unittest` case reads the three
files with regular expressions and asserts the three sets are equal, with the
assertion message naming each version present in one place and missing from
another, and `.travis.yml` is deleted → a developer reading PyPI or the README
sees only CI-verified versions, and a maintainer who edits one place is told
by the suite to edit the others.

## Current decisions

- Existing solutions (PFE): no module or test in the repository compares
  package metadata with CI configuration; the three claims are maintained by
  hand today. The agreement test is the only new concept, placed in `test/`
  beside the other repository-level tests, with no new dependency.
- The matrix stays the single source the other two follow; the test enforces
  agreement rather than generating the classifiers or the README, because
  neither the workflow nor the README can import Python at authoring time.
- Slice order: the CI probe (slice 1) precedes the claims (slice 2), so the
  claims never name a version CI has not run green. After slice 1 alone,
  every claimed version is verified and the suite still passes, which is the
  safe stopping point.
- Local gate: `AGENTS.md` requires the full suite before completing a change
  and `nix develop -c make` for all checks; both run at every slice boundary.
  Hosted CI's matrix is the proof of slice 1, not a local gate.
- Test naming follows the existing `test*.py` convention required by
  `setup.cfg` (`python_files = test*`); the test file must run on Python 3.8.
- No ADR concern: this changes packaging metadata, CI configuration and
  documentation, not domain vocabulary or product structure.

## Learnings

None yet. Record here any interpreter that failed in CI with the job name and
the first failing test, and the reduced set carried into slice 2.

## Concern evidence

Slice-plan refinement was not needed: two slices, each with one proof loop,
one Behavior each, no Structure, no shared special cases. Remaining
slice-specific concerns: none. The CI wait in slice 1 is an accepted
external-wait exception the plan names, with the story's boundary rule as its
failure handling.
