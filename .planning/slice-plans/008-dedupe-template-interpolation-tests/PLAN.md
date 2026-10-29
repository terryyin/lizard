# Dedupe template-interpolation CCN tests

**Mapped story identity:** SEED-019#dedupe-template-interpolation-tests
**Source:** [canonical story](../../seeds/SEED-019-template-interpolation-conditions.md#dedupe-template-interpolation-tests)
**Source review:** [source-review.md](source-review.md)
**Provenance:** SEED-019#template-interpolation-conditions execution retrospective;
related commits `9ff15db`, `59a5131`, published `6f5638b`, merge `d0ecc33`.

## Goal and scope

For maintainers running the JS/TS language suite, the overlapping
`test_conditions_in_interpolations` scenario is owned once by
`testTypeScriptInterpolationConditions.py` (with story `label`, nested/literal,
and keyword-member controls). Remove the residual duplicate class left in
`testTypeScript.py` when #545 published the extracted file without deleting the
contributor-head copy.

**In scope:** Delete
`Test_conditions_in_template_interpolations` from
`test/test_languages/testTypeScript.py` only.
**Excluded:** Product readers; redesign of other TypeScript tests; backlog Take.
**Architecture / PFE:** Test-suite cleanup only; no reader or ADR change.
**Assumptions:** Surviving coverage in
`testTypeScriptInterpolationConditions.py` remains the authoritative home for
interpolation CCN behavior (includes the a/b/c case plus stronger story/guard
cases).

## Outside-in proof

Focused
`nix develop -c python -m pytest test/test_languages/testTypeScript.py test/test_languages/testTypeScriptInterpolationConditions.py -q`
shows green; collection lists
`Test_conditions_in_template_interpolations` only under
`testTypeScriptInterpolationConditions.py`.

## Settled decisive premises

Observed in this execution checkout against `origin/master` at `d0ecc33`
(2026-10-29):

| Premise | Consuming slices | Observation | Result |
| --- | --- | --- | --- |
| Trunk still carries both class homes | 1 | `git show origin/master:…/testTypeScript.py` and `…/testTypeScriptInterpolationConditions.py` both define `Test_conditions_in_template_interpolations` | Defect remains |
| Extracted file owns the stronger suite | 1 | Interpolation file has `test_story_label_…`, nested/literal controls, keyword-member guard; `testTypeScript.py` has only `test_conditions_in_interpolations` | Safe to delete TypeScript.py class |
| Story tip already removed the residual | — | Story branch `59a5131` `testTypeScript.py` lacks the class; trunk merge path did not | Correction targets trunk / next delivery, not story-tip product |

## Ordered slices

### 1. Drop residual duplicate interpolation CCN class
Type: Structure
Status: done
Proof: Focused pytest on the two TypeScript test modules green; only
`testTypeScriptInterpolationConditions.py` collects
`Test_conditions_in_template_interpolations`.
Behavior: Given trunk after #545 still defines the overlapping class in
`testTypeScript.py` → delete that class only → the extracted module remains the
sole owner and focused TypeScript language tests pass.
Safe stop: Suite cost reduced; product CCN behavior unchanged.

Accepted proof (slice 1):
```
proof:
  command: nix develop -c python -m pytest test/test_languages/testTypeScript.py test/test_languages/testTypeScriptInterpolationConditions.py -q
  covers: remaining testTypeScript.py suite + authoritative interpolation CCN suite
  boundary: JS/TS language test modules; no product reader edit
  observations:
    - testTypeScriptInterpolationConditions.py::test_conditions_in_interpolations assertEqual([('a', 4), ('b', 3), ('c', 1)], …)
    - testTypeScript.py: no Test_conditions_in_template_interpolations (grep empty)
    - collect-only: class only under testTypeScriptInterpolationConditions.py (4 methods)
  setup: none
  result: pass (23 passed)
```
Refactor: none — already clean (`## REFACTOR COMPLETE`).

## Current decisions

- One Structure slice; no product edit.
- Surviving proof home is `testTypeScriptInterpolationConditions.py`.
- Do not re-open interpolation CCN product scope.

## Learnings

- Contributor-head publish that copies a new extracted test file must also
  remove the pre-extract class from `testTypeScript.py`, or trunk keeps both.

## Concern evidence

- Slice-plan refinement: not needed — single Structure slice, proof mapped,
  decisive premises observed.
- Remaining slice-specific concerns: none identified.

## Execution complete

Product advice: no change — residual duplicate class removed; authoritative
interpolation suite unchanged; no further product backlog recommendation from
this cleanup. Process finding DD-017 recorded (first-landing expectation vs
story-branch increment publish); no correction plan required for the story
outcome.
