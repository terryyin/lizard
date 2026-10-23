---
id: SEED-006
status: active
planted: 2026-10-23
planted_during: Story wrap-up product advice after the README began stating the CI-verified Python versions
trigger_when: A developer installs Lizard on a Python version newer than 3.10, or a maintainer changes the CI matrix or the package classifiers
scope: small
---

# SEED-006: Python version support

The README states that the test suite runs in continuous integration on
Python 3.8 and 3.10, on Linux and Windows. That is the `python-version` matrix
in `.github/workflows/python-app.yml`. The `setup.py` classifiers claim
support for Python 3.8, 3.9, 3.10 and 3.11, so PyPI advertises versions that
CI never runs. Newer interpreters are not covered at all.

<a id="ci-verified-python-support"></a>

### Claim support only for the Python versions CI verifies

**Identity:** SEED-006#ci-verified-python-support
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/003-ci-verified-python-support/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"3051d16f7655dd3094b58b4dc988b4182cb11042ea29ca49ecd316982ef7278d","plan":"0672becdb6b15f810a92c28976c3693fda8cd351190c904c78e8f0c353b0fed6"}}
```

**Goal:** A developer choosing an interpreter can trust that every Python
version Lizard claims to support, on PyPI and in the README, is one its test
suite runs on in continuous integration. Direction: both widen and align. CI
verifies every CPython minor from the current floor to the newest release,
and the claims name exactly that set.

**Scope:**

Required behavior:

- The `python-app.yml` matrix runs the test suite on CPython 3.8, 3.9, 3.10,
  3.11, 3.12, 3.13 and 3.14, each on `ubuntu-latest` and `windows-latest`.
  3.8 stays the floor: raising it would remove support, a decision this story
  does not make. 3.14 is the newest released minor at refinement
  (2026-10-23). 3.9 is also the `nix develop` interpreter, so the developer's
  own environment becomes a verified one.
- The `setup.py` `Programming Language :: Python :: 3.x` classifiers name
  exactly the versions the matrix runs.
- The README's verified-versions sentence names exactly those versions, on
  Linux and Windows, and keeps saying other versions are not verified.
- One test in `test/` reads the workflow matrix, the classifiers and the
  README sentence and fails naming the difference when they disagree, so a
  maintainer who changes one is told to change the others. Justified by this
  seed's trigger ("a maintainer changes the CI matrix or the package
  classifiers") and `AGENTS.md` principle 6.
- `.travis.yml`, which claims a Travis verification on 3.8, 3.10 and PyPy
  that no longer runs (its badge was already removed as dead under
  SEED-001#dead-travis-badge, and nothing references the file), is deleted
  so no retired CI configuration claims verified versions.

Implementation detail, not extra scope: the workflow's
`python setup.py build install` step becomes `python -m pip install .`,
because a fresh Python 3.12 or later interpreter ships no `setuptools`
(observed on a fresh 3.14 venv during refinement: the import fails, while
`pip install .` installs and runs Lizard).

Boundary rule (from this seed: a failure on a newly verified interpreter is
a separate fix): if the widened matrix fails on an interpreter on Linux or
Windows, that version leaves the matrix, the classifiers and the README
together, and the failure is queued as its own story. This story changes no
analyzer behavior.

Deferred promises, neither built nor verified here:

- `python_requires` metadata. It would be a new pip-enforced claim about
  versions below 3.8, not an alignment of existing claims.
- PyPy or any non-CPython interpreter.
- Upgrading `actions/checkout@v3` and `actions/setup-python@v3`. Done only if
  the widened run cannot find an interpreter otherwise.
- The release workflow's build interpreter (3.11) is not a support claim and
  stays.
- Future CPython releases (3.15 and later): a maintainer adds them to the
  matrix, and the agreement test then demands the matching claims.

**Key examples:**

1. README says the suite runs on Python 3.8 and 3.10, and the classifiers
   list 3.8 to 3.11 → a developer on Python 3.13 reads the README and the
   PyPI classifiers → both list 3.8, 3.9, 3.10, 3.11, 3.12, 3.13 and 3.14 on
   Linux and Windows, and the workflow matrix runs exactly those on both.
2. The workflow matrix lists 3.8 and 3.10 and installs through `setup.py` →
   a commit is pushed → the "Python application" run has 14 green jobs, one
   per version and OS, each installing Lizard with pip and running the suite.
3. Matrix, classifiers and README agree → a maintainer adds `"3.15"` to the
   matrix only and runs `pytest` → one test fails, naming 3.15 as present in
   the workflow but missing from the classifiers and the README.
4. `.travis.yml` lists 3.8, 3.10 and PyPy → a maintainer looks for CI
   configuration → only `.github/workflows/` remains; no file claims a
   Travis verification.
5. Boundary: the matrix is widened → CI fails on one interpreter, say 3.14 on
   Windows → 3.14 leaves the matrix, the classifiers and the README together,
   a separate story records the failure, and the agreement test stays green.
