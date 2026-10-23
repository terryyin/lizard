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
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

**Goal:** A developer choosing an interpreter can trust that every Python
version Lizard claims to support, on PyPI and in the README, is one its test
suite runs on in continuous integration.

**Initial scope (not yet refined):**

- Decide which interpreters CI should verify: widen the matrix to the
  supported or newer versions, narrow the classifiers to what CI runs, or
  both.
- Keep the CI matrix, the `setup.py` classifiers and the README's
  verified-versions sentence in agreement.
- No analyzer behavior changes are intended. A failure on a newly verified
  interpreter becomes a separate fix if it appears.
