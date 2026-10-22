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

<a id="metric-list-punctuation"></a>

### Read the list of counted metrics with consistent punctuation

**Identity:** SEED-001#metric-list-punctuation
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"16d88895ab6a269a2fd9af58c1a496841d7af1c20fda4be7c7233ac7cfcc1a67"}}
```

**Goal:** A developer scanning what Lizard counts reads one list punctuated
the same way on every line.

#### Scope

- In `README.rst`, the “It counts” list: the third item ends with a comma
  like the first two, and only the last item ends with a full stop.
- The items and their wording stay as they are.

#### Key examples

- “-  token count of functions.” → “-  token count of functions,”
- “-  parameter count of functions.” stays as it is: the last item keeps its
  full stop.
- The first two items, “the nloc (lines of code without comments),” and
  “CCN (cyclomatic complexity number),”, already end with a comma and stay as
  they are.

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
