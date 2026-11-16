---
id: SEED-057
status: active
planted: 2026-11-16
planted_during: lizard process issue
scope: cognitive_complexity missing when score is 0
---

# SEED-057: Fix issue #570 cognitive_complexity absent at zero

<a id="fix-issue-570-cognitive-complexity-zero"></a>

### Fix issue #570 — cognitive_complexity present when score is 0

**Identity:** SEED-057#fix-issue-570-cognitive-complexity-zero
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"f557ccf28d1bb99f6c2f8ac9f1a8555ee38e13b3c1411fb9067bae2a96f86b1c"}}
```

**Goal:** Users who activate the cognitive complexity extension through the
Python API, and maintainers closing
[issue #570](https://github.com/terryyin/lizard/issues/570), get one complete
repair: every analyzed function exposes `cognitive_complexity` on the function
object (including `__dict__`) when the extension is active, with value `0` for
functions the CLI already prints as CogC 0, matching non-zero functions that
already carry the attribute.

**Source:** Dashboard instruction `lizard process issue`
https://github.com/terryyin/lizard/issues/570. Reporter asks whether absence at
zero is intentional; no contributor PR is expected. Bounded planless repair
under `lizard-process-issue` / `dough-bug-fixing`, then qualify the full repair
with `lizard-process-pr --local-change` before landing or wrap-up.

**Beneficiary:** API consumers who read `cognitive_complexity` from function
results without special-casing missing keys, and maintainers who want API and
CLI CogC reporting to agree.

**Scope:** Cognitive complexity extension attribute presence for score 0 only.
Reproduce the missing-key case for a CogC-0 function via the public analyzer
with the extension active; make the smallest change so zero and non-zero
functions both expose `cognitive_complexity`; keep CLI CogC printing and
non-zero scoring. Qualify the aggregate repair before delivery. Deferred:
other extension attributes, cognitive scoring formula changes, and unrelated
output formats.

**Key examples:**

- Given a simple function whose CLI `-Ecognitive` output shows CogC 0, the
  Python API with the cognitive extension active yields
  `function.cognitive_complexity == 0` and `"cognitive_complexity" in
  function.__dict__`.
- Given a function whose cognitive score is non-zero, the same attribute
  remains present with that score.
- Given analysis without the cognitive extension, functions do not gain a
  spuriously invented `cognitive_complexity` attribute.

**Starting revision:** Established session
`153d629fc455e4fef1788e22883fbffac89287ab` on
`cursor/lizard-process-issue-https-github-com-terryyin-l-2`.

**Qualification:** After the shared refactor and before delivery or wrap-up,
qualify the entire repair with `lizard-process-pr --local-change`.
