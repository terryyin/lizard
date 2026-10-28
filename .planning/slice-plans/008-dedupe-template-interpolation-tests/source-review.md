# Source review — SEED-019#dedupe-template-interpolation-tests

**Launch:** Fresh general-purpose agent (no conversation inheritance); source-only;
read no other project files; change nothing.
**Input:** Story Goal, Scope, Key examples, and exclusions only (no plan, return,
product, or proof).

## Reviewer return

### Commitments

1. For maintainers running the JS/TS language suite, `test_conditions_in_interpolations` runs once under `testTypeScriptInterpolationConditions.py`.

2. `testTypeScriptInterpolationConditions.py` remains the owner of that scenario’s supporting coverage:
   - 2.1 story `label`
   - 2.2 nested/literal/keyword-member controls
   - 2.3 the overlapping a/b/c case

3. There is no second identical-class case left in `testTypeScript.py` from the #545 merge publish path.

4. On trunk, delete only the residual `Test_conditions_in_template_interpolations` class from `test/test_languages/testTypeScript.py`.

5. Preserve all cases in `testTypeScriptInterpolationConditions.py`.

6. Make no product-parser changes.

### Qualifications

- Q1. Audience is maintainers running the JS/TS language suite.
- Q2. The duplicate to remove is the residual identical class in `testTypeScript.py` associated with the #545 merge publish path.
- Q3. Deletion is limited to that residual class in `testTypeScript.py` on trunk (“only”).
- Q4. Ownership of story `label`, nested/literal/keyword-member controls, and the overlapping a/b/c case already resides in `testTypeScriptInterpolationConditions.py` and is preserved, not relocated by this correction.

### Trigger / journey examples

- E1. Given trunk after the #545 merge, both `testTypeScript.py` and `testTypeScriptInterpolationConditions.py` define `Test_conditions_in_template_interpolations` with `test_conditions_in_interpolations`.
- E2. After the correction, only `testTypeScriptInterpolationConditions.py` owns that class.
- E3. Focused TypeScript language tests still pass.

### Exclusions

- X1. No product-parser changes.
- X2. Do not leave a second identical-class case in `testTypeScript.py`.
- X3. Do not remove or alter cases in `testTypeScriptInterpolationConditions.py`.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | For maintainers running the JS/TS language suite, `test_conditions_in_interpolations` runs once under `testTypeScriptInterpolationConditions.py`. | slice 1 | observed: collect-only lists that method only under testTypeScriptInterpolationConditions.py; testTypeScript.py has no class |
| 2.1 | Owner of supporting coverage: story `label` | slice 1 | observed: testTypeScriptInterpolationConditions.py::test_story_label_reports_ccn_3_on_each_extension |
| 2.2 | Owner of supporting coverage: nested/literal/keyword-member controls | slice 1 | observed: test_nested_follow_and_literal_controls; test_keyword_member_inside_interpolation_is_not_a_condition |
| 2.3 | Owner of supporting coverage: the overlapping a/b/c case | slice 1 | observed: testTypeScriptInterpolationConditions.py::test_conditions_in_interpolations assertEqual([('a', 4), ('b', 3), ('c', 1)], …) |
| 3 | No second identical-class case left in `testTypeScript.py` from the #545 merge publish path. | slice 1 | observed: git diff deletes only Test_conditions_in_template_interpolations from testTypeScript.py; grep finds class only in interpolation file |
| 4 | On trunk, delete only the residual `Test_conditions_in_template_interpolations` class from `test/test_languages/testTypeScript.py`. | slice 1 | observed: git diff --name-only is only testTypeScript.py; −24 lines class + method only |
| 5 | Preserve all cases in `testTypeScriptInterpolationConditions.py`. | slice 1 | observed: file unchanged in working tree; four methods still present |
| 6 | Make no product-parser changes. | slice 1 | observed: git diff --name-only excludes lizard_languages/ and product paths |

## Acceptance comparison (slice 1)

| Id | Trigger / setup | Source-expected | Actual | Disposition |
| --- | --- | --- | --- | --- |
| E1→E2 | Trunk after #545 had both class homes | Only interpolation file owns the class | Class present only in testTypeScriptInterpolationConditions.py; removed from testTypeScript.py | observed slice 1 |
| E3 | After deletion | Focused TS language tests pass | `pytest …testTypeScript.py …testTypeScriptInterpolationConditions.py -q` → 23 passed | observed slice 1 |
| Q1–Q4 | Maintainers / residual-only / preserve ownership | As qualifications | Unchanged ownership in interpolation file; deletion limited to residual class | observed slice 1 |
| X1–X3 | — | No parser edits; no residual class; preserve interpolation cases | Diff and files match | observed slice 1 (via 3, 5, 6) |
