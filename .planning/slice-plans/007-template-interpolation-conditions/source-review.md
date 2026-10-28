# Source review — SEED-019#template-interpolation-conditions

## Launch and input

Isolated general-purpose reviewer (no conversation inheritance, no repo
reads). Input was only the selected story's Goal, Scope, key examples,
exclusions, evaluation, and completion text from
`.planning/seeds/SEED-019-template-interpolation-conditions.md#template-interpolation-conditions`.
No plan, implementation return, product, or proof was supplied.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | For JS/TS/JSX/TSX analyzer users who rely on CCN and warning thresholds, conditions written inside template interpolations contribute to each function's CCN the same way they do outside templates. | slice 1 | observed: test/test_languages/testTypeScriptInterpolationConditions.py::Test_conditions_in_template_interpolations::test_story_label_reports_ccn_3_on_each_extension (label CCN 3 ×4); test_nested_follow_and_literal_controls outside control; docs/reviews/fabio-malpezzi-20261027/pr545/inputs.json 16/16 |
| 2 | PR #545 lands only after that fix is performance-qualified under the review contract, with one of these outcomes preserved as alternatives | slice 2 | observed: https://github.com/terryyin/lizard/pull/545 MERGED after attempt10 six-gate pass; thanks https://github.com/terryyin/lizard/pull/545#issuecomment-6483363609 |
| 2.1 | exact-head merge and contributor thanks | slice 2 | observed: exact-head merge of 6f5638b116f783970fecaabd0efc358330e0d47d → mergeCommit d0ecc33; thanks URL retained |
| 2.2 | an evidenced further deferral if the gate still fails after an eligible hot-path trim | slice 2 | observed: unused alternative — gates passed after eligible trim (attempt10) |
| 3 | Own PR #545 through a complete disposition under REVIEW-CONTRACT.md. | slice 2 | observed: REVIEW-CONTRACT disposition merged-with-acknowledgement in attempt10/disposition.json |
| 3.q1 | Frozen contributor head: baa95a4df8278421bb3d3a6487516030cc03a979 | slice 2 | observed: refreshed before publish; published corrections onto prior frozen head baa95a4 → 6f5638b |
| 3.q2 | Still OPEN at refinement (state as of this source). | slice 2 | observed: was OPEN at action; now MERGED |
| 4 | Reuse the adapted candidate and review record under pr545/. | slice 1 | observed: lizard_languages/javascript_templates.py::interpolation_code; typescript.py::_count_interpolation_conditions; tsx.py::_scan_atomic_templates=True (concept merged onto trunk, not blind drop) |
| 4.q1 | adapted.diff is DISPLAY-ONLY. | slice 1 | observed: no adapted.diff applied as product; design taken from literal-git-patches / raw.diff concepts |
| 4.q2 | Executable bytes are in literal-git-patches.json. | slice 1 | observed: re-adaptation referenced docs/reviews/fabio-malpezzi-20261027/pr545/literal-git-patches.json / raw.diff |
| 5 | Count conditions inside `${…}` on the TypeScriptReader fast path via interpolation_code. | slice 1 | observed: lizard_languages/typescript.py::__call__ fast path calls _count_interpolation_conditions; javascript_templates.py::interpolation_code |
| 6 | Limit atomic-template backtick scans to TSX/JSX. | slice 1 | observed: typescript.py::_scan_atomic_templates=False; tsx.py::_scan_atomic_templates=True |
| 7.1 | Required labeled behavior: analysis of a function returning `${x ? 1 : 2} ${x && y}` reports CCN 3 on .js | slice 1 | observed: test_story_label_reports_ccn_3_on_each_extension subTest label.js; analyze_source_code → ('label', 3) |
| 7.2 | … on .ts | slice 1 | observed: same class subTest label.ts → ('label', 3) |
| 7.3 | … on .jsx | slice 1 | observed: same class subTest label.jsx → ('label', 3) |
| 7.4 | … on .tsx | slice 1 | observed: same class subTest label.tsx → ('label', 3) |
| 8.1 | Keep nested templates correct when interpolation counting is added | slice 1 | observed: test_conditions_in_interpolations function b CCN 3; test_nested_follow_and_literal_controls nested case |
| 8.2 | Keep literal text correct | slice 1 | observed: test_conditions_in_interpolations function c CCN 1; literal-controls cases |
| 8.3 | Keep strings correct | slice 1 | observed: test_conditions_in_interpolations / literal controls with `'&& ?'` string |
| 8.4 | Keep comments correct | slice 1 | observed: comment controls in test_nested_follow_and_literal_controls and function c |
| 8.5 | Keep regex correct | slice 1 | observed: regex `.test` / replace cases in test_nested_follow_and_literal_controls |
| 8.6 | Keep following function boundaries correct | slice 1 | observed: nested-follow cases leave following `b` at CCN 1 |
| 9.1 | Land only when all six contract performance comparisons' paired 95% upper bounds are strictly below 3% on a quiet machine | slice 2 | observed: attempt10/measurement-summary.json all six paired_95pct_ci upper <3% (max +2.449%) |
| 9.2 | or after a ≤15-minute hot-path trim that achieves that same six-gate result | slice 2 | observed: attempt10 trim path used; gates passed |
| 9.q1 | Performance harness identity: SHA256 ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520 | slice 2 | observed: paired.py SHA256 ec1c9c5e…76520 asserted in measure.py / resume script |
| 10 | After a qualifying pass: exact-head merge and thank the contributor. | slice 2 | observed: merge + thanks https://github.com/terryyin/lizard/pull/545#issuecomment-6483363609 |
| 11 | Refresh PR state/head and current target before acting. | slice 2 | observed: refreshed PR/head/target before publish and merge (master ancestor of 6f5638b) |
| 12 | After the interpolation path is added, keep `${p.catch(f)}` (and similar) as non-conditions. | slice 1 | observed: test_keyword_member_inside_interpolation_is_not_a_condition → ('a', 1) ×4; analyze_source_code catch guard |
| 12.q1 | #546 already landed the keyword-member fix on trunk. | slice 1 | observed: typescript.py still undoes keyword members on outer stream and inside _count_interpolation_conditions |
| 12.q2 | This story does not reopen keyword-member scope. | slice 1 | observed: no keyword-member scope expansion beyond interp-path guard |
| 13.1 | Completion is merged acknowledgement on #545 | slice 2 | observed: MERGED acknowledgement on #545 |
| 13.2 | or a linked further follow-up when the gate still fails after an eligible trim | slice 2 | observed: unused alternative — merged |
| 14.q1 | attempt8's incremental names/counts failure (+4.767% upper) is current evidence against the adapted candidate as-is. | slice 1 | G3 |
| 14.q2 | A justified quiet longer retry already ran. | slice 1 | G3 |
| 14.q3 | ADR-0001 governs CCN meaning. | slice 1 | G4 |
| 14.q4 | ADR-0000 keeps human architectural ownership — no new Architecture section for this extension of existing reader behavior. | slice 1 | G4 |
| 15 | Given label on current trunk CCN 1 ×4; after the qualified fix it reports CCN 3 on each extension. | slice 1 | observed: post-fix analyze_source_code and test_story_label_reports_ccn_3_on_each_extension → CCN 3 ×4 (performance qualification of that fix remains slice 2) |
| 16 | Given nested interpolations, analysis counts both conditions (CCN 3) without breaking the following function's boundary. | slice 1 | observed: test_conditions_in_interpolations b=3; nested-follow boundary cases |
| 17 | Given interpolations whose apparent operators are only in string/regex/comment literals, those literals do not inflate CCN; a real condition in a regex `.test` still counts. | slice 1 | observed: function c CCN 1; regex `.test` case CCN 2 in test_nested_follow_and_literal_controls |
| 18 | Given the same ternary/`&&` expressions outside a template, CCN stays correct. | slice 1 | observed: outside control in test_nested_follow_and_literal_controls → ('outside', 4); inputs.json outside fixtures |
| 19 | Given a candidate that restores CCN 3 but whose incremental names/counts paired 95% upper bound is ≥3%, the story does not merge; trim ≤15 minutes or linked further deferral. | slice 2 | observed: attempt9 failed gate then ≤15-min trim → attempt10 pass; no merge of failing candidate |
| 20 | Evaluation path: reproduce target red → corrected green on the pr545 fixtures; focused JS/TS suite including reached consumers; full suite/`make`; six-gate harness; required CI; then exact-head merge and thanks — or evidenced further deferral. | slice 2 | observed: fixtures/focused/full pytest/make/six-gate/CI then exact-head merge+thanks |
| 21 | Deferred: unrelated language readers. | slice 1 | G1 |
| 22 | Deferred: redesign of the performance harness. | slice 1 | G2 |
| 23 | Do not merge without all six gates' paired 95% upper bounds strictly below 3%. | slice 2 | observed: merge only after attempt10 all six upper <3% |
| 24 | Do not repeatedly time the same invalid or already-failed candidate to seek a pass. | slice 2 | observed: attempt10 new trimmed bytes; did not retime attempt8/9 failed candidates |
| 25 | Do not invent a parallel parser. | slice 1 | observed: extension of TypeScriptReader / javascript_templates only; no new parser module |
