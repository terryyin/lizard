---
id: SEED-001
status: active
planted: 2026-10-13
planted_during: Selection of small, independent README corrections
trigger_when: A developer reads the README to decide how to install or use Lizard
scope: small
---

# SEED-001: README accuracy

A developer evaluating or adopting Lizard reads `README.rst` first, on GitHub
and on PyPI. Several statements there are outdated or awkward. Each story
below corrects one of them and is independent of the others. None changes
analyzer behavior.

<a id="verified-python-versions"></a>

### Know which Python versions Lizard is verified on

**Identity:** SEED-001#verified-python-versions
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/001-verified-python-versions/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"2afe904d47b32db5e55d89c253598c47aa1ecaf467be307a8e04a82a2beb9e29","plan":"fbee23d751df7c954c79c1baf12f38507d3fc598e698ddd5a7495f14bc43b281"}}
```

**Goal:** A developer choosing an interpreter reads in `README.rst` which
Python versions Lizard's test suite is run on, so the README claims exactly
what continuous integration verifies.

#### Scope

- In `README.rst`, the sentence “It requires python3.8 or above (early
  versions are not verified).” becomes one statement of the minimum version
  and of the versions the test suite runs on in continuous integration:
  Python 3.8 and 3.10, on Linux and Windows. Other versions stay unverified
  and the README says so.
- The verified versions are the `python-version` entries of the CI matrix in
  `.github/workflows/python-app.yml`. The README names them literally; it
  does not derive or generate them.
- The CI matrix and the `setup.py` classifiers (3.8 to 3.11) stay as they
  are. Widening the matrix to newer interpreters and aligning the
  classifiers with it were considered: they change what is verified rather
  than what the README reports, so they are deferred to a separate story if
  wanted.
- Boundary assumptions: `.travis.yml` is a retired configuration (its badge
  was removed by an earlier story) and is not a verification source. The
  sentence stays plain reStructuredText so PyPI, which renders
  `README.rst` as the long description, shows it unchanged.
- No analyzer behavior changes.

#### Key examples

- The README says “It requires python3.8 or above (early versions are not
  verified).” → a developer reads the introduction → they read “It requires
  Python 3.8 or above. The test suite runs on Python 3.8 and 3.10, on Linux
  and Windows, in continuous integration; other versions are not verified.”
- The CI workflow on `master` runs four jobs, `build (ubuntu-latest, 3.8)`,
  `build (ubuntu-latest, 3.10)`, `build (windows-latest, 3.8)` and
  `build (windows-latest, 3.10)`, and all four passed on run 39877134491 →
  the README names exactly 3.8 and 3.10 as verified, no more and no fewer.
- The minimum stays 3.8: “or above” keeps the current requirement claim, and
  the `setup.py` classifiers are not edited.
