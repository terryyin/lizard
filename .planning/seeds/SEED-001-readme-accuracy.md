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

<a id="exit-code-wording"></a>

### Read the exit-code rule in plain wording

**Identity:** SEED-001#exit-code-wording
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** A developer wiring Lizard into a build reads, without stumbling,
that warnings make the exit code non-zero.

#### Scope

- In `README.rst`, the sentence about the exit code says “non-zero” in place
  of “none-Zero”.
- No other wording changes.

#### Key examples

- “The exit code of lizard will be none-Zero if there are warnings.” → “The
  exit code of lizard will be non-zero if there are warnings.”

<a id="python3-module-example"></a>

### Copy the Python module example into Python 3

**Identity:** SEED-001#python3-module-example
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** A developer using Lizard as a Python module can paste the README
example into a supported Python 3 interpreter without a syntax error.

#### Scope

- In `README.rst`, “Using lizard as Python module”, the two `print`
  statements become `print(...)` calls.
- The sample output lines and the surrounding prose stay as they are.

#### Key examples

- `>>> print i.__dict__` → `>>> print(i.__dict__)`
- `>>> print i.function_list[0].__dict__` →
  `>>> print(i.function_list[0].__dict__)`

<a id="dead-travis-badge"></a>

### See no build badge for a retired service

**Identity:** SEED-001#dead-travis-badge
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** A developer judging the project's health from the README header is
not shown a build badge for travis-ci.org, a service that no longer runs this
project's builds.

#### Scope

- In `README.rst`, remove the Travis CI image and its target from the header.
- The PyPI badge and the web-site logo stay.
- Adding a replacement badge is outside this story.

#### Key examples

- The header shows the PyPI version badge and the logo, and no
  `travis-ci.org` reference remains in `README.rst`.

<a id="research-section-wording"></a>

### Read the research section in idiomatic English

**Identity:** SEED-001#research-section-wording
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless"}
```

**Goal:** A researcher looking for prior work that used Lizard finds the
section under a heading and introduction in idiomatic English.

#### Scope

- In `README.rst`, the heading “Literatures Referring to Lizard” becomes
  “Literature Referring to Lizard”, with its underline adjusted to the new
  length.
- In the sentence below it, “software related researches” becomes
  “software-related research”.
- The listed references stay as they are.

#### Key examples

- “Lizard is often used in software related researches.” → “Lizard is often
  used in software-related research.”

<a id="verified-python-versions"></a>

### Know which Python versions Lizard is verified on

**Identity:** SEED-001#verified-python-versions
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"unselected"}
```

A developer choosing an interpreter reads “It requires python3.8 or above
(early versions are not verified)” in `README.rst`. The package metadata in
`setup.py` lists Python 3.8 to 3.11, and the CI workflow
`.github/workflows/python-app.yml` runs 3.8 and 3.10. The README does not say
which versions are actually verified, and the three sources disagree.

The developer should be able to tell, from the README, which Python versions
Lizard is verified on. What the README should claim, and whether the metadata
or CI matrix should change with it, is not decided yet.
