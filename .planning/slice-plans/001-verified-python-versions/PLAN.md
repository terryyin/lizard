# Plan: Know which Python versions Lizard is verified on

**Identity:** SEED-001#verified-python-versions

**Source:** [Know which Python versions Lizard is verified on](../../seeds/SEED-001-readme-accuracy.md#verified-python-versions)

## Goal and scope

A developer choosing an interpreter reads in `README.rst` which Python
versions Lizard's test suite is run on, so the README claims exactly what
continuous integration verifies.

Included: replace the one README sentence “It requires python3.8 or above
(early versions are not verified).” with a statement of the minimum version
and of the versions the CI matrix runs, 3.8 and 3.10 on Linux and Windows.

Excluded: the CI matrix in `.github/workflows/python-app.yml`, the `setup.py`
classifiers, and `.travis.yml` stay unchanged; widening or aligning them is
deferred to a separate story. No analyzer behavior changes.

Assumptions: the README names the versions literally; nothing derives them
from the workflow. The sentence stays plain reStructuredText so PyPI renders
it as the long description.

## Outside-in proof

| Promise (key example) | Owning slice | Observable proof |
| --- | --- | --- |
| The introduction states the minimum and the verified versions | 1 | `grep -n "The test suite runs on Python 3.8 and 3.10" README.rst` prints the new sentence and `grep -n "python3.8 or above" README.rst` prints nothing |
| Exactly the CI matrix versions are named | 1 | The sentence names 3.8 and 3.10, the only `python-version` entries in `.github/workflows/python-app.yml` |
| The minimum stays 3.8 and classifiers are untouched | 1 | `git diff --stat` touches only `README.rst`; the sentence keeps “Python 3.8 or above” |
| The README still renders on PyPI | 1 | `nix develop -c make` stays green (the project's pre-completion gate per `AGENTS.md`); the edit adds no RST directive |

## Decisive premises

| Premise | Consuming operation | Observation | Result |
| --- | --- | --- | --- |
| The CI matrix runs Python 3.8 and 3.10 on ubuntu and windows and passes on `master` | The claim the new sentence makes | `gh run view 39877134491 --json jobs` in this checkout's repository | All four jobs (`ubuntu-latest`/`windows-latest` × `3.8`/`3.10`) concluded `success`; settled |
| The old sentence exists verbatim once in `README.rst` | The replacement edit | `grep -n "python3.8 or above" README.rst` in this workspace | One match at line 66; settled |

## Slices

### 1. README states the verified Python versions
Type: Behavior
Status: planned
Proof: `grep -n "The test suite runs on Python 3.8 and 3.10" README.rst` prints the sentence; `grep -n "python3.8 or above" README.rst` prints nothing; `git diff --stat` names only `README.rst`; `nix develop -c make` green.

Behavior: `README.rst` says “It requires python3.8 or above (early versions
are not verified).” → the sentence is replaced → it says “It requires Python
3.8 or above. The test suite runs on Python 3.8 and 3.10, on Linux and
Windows, in continuous integration; other versions are not verified.”

## Current decisions

- Existing solutions (PFE): the README introduction is the only place that
  states interpreter requirements, so the sentence is changed in place; no new
  responsibility is added or relocated and no Accepted ADR applies.
- This preparation established the executable-plan root for the project at
  `.planning/slice-plans/<NNN>-<slug>/PLAN.md`, recorded in
  `.agents/agent-map.md`.

## Learnings

None yet.

## Concern evidence

Slice-plan refinement was not needed: the plan has one Behavior slice with
one proof loop and no boundary to consolidate or split. This review
identified no remaining slice-specific concern.
