---
id: SEED-019
status: active
planted: 2026-10-28
planted_during: SEED-010#review-lexical-js-prs wrap-up product advice
trigger_when: Remove the duplicate template-interpolation CCN test left on trunk after #545
scope: small
---

# SEED-019: Template-interpolation condition counting

## Why This Matters

JS/TS/JSX/TSX conditions inside template interpolations contribute to CCN on the
`TypeScriptReader` path (`interpolation_code`, with atomic-template scans limited
to TSX/JSX). PR #545 landed that behavior under the review performance contract.
Evidence for that disposition remains under
[docs/reviews/fabio-malpezzi-20261027/pr545/](../../docs/reviews/fabio-malpezzi-20261027/pr545/).
One residual remains: a duplicate test class on trunk.

## Selected Stories

<a id="dedupe-template-interpolation-tests"></a>

### 1. Remove the duplicate template-interpolation CCN test on trunk

**Identity:** SEED-019#dedupe-template-interpolation-tests
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/008-dedupe-template-interpolation-tests/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"4d6235f1c7e32b63cdd8dd273920515fbe49b88b349a8310fe0363f007846407","plan":"784440d23de2ab55147142028f452540479fb869b1ec5649e732a667f1d87b80"}}
```

**Goal:** For maintainers running the JS/TS language suite, the
`test_conditions_in_interpolations` scenario runs once under
`testTypeScriptInterpolationConditions.py`, which already owns story `label`,
nested/literal/keyword-member controls, and the overlapping a/b/c case — without
a second identical-class case left in `testTypeScript.py` from the #545 merge
publish path.

**Scope:** Delete only the residual
`Test_conditions_in_template_interpolations` class from
`test/test_languages/testTypeScript.py` on trunk. Preserve all cases in
`testTypeScriptInterpolationConditions.py`. No product-parser changes.

**Key examples:**

- Given trunk after #545 merge, both `testTypeScript.py` and
  `testTypeScriptInterpolationConditions.py` define
  `Test_conditions_in_template_interpolations` with
  `test_conditions_in_interpolations`; after the correction only the latter
  file owns that class, and focused TypeScript language tests still pass.
