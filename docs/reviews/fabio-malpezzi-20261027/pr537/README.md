# PR537 arrow parameters after member access

Disposition: **merged** under SEED-010#review-structural-js-prs slice 1 as
[`6a2b8ea91509ef44f32925cbf16d75f7cadc6db3`](https://github.com/terryyin/lizard/commit/6a2b8ea91509ef44f32925cbf16d75f7cadc6db3), with
[acknowledgement](https://github.com/terryyin/lizard/pull/537#issuecomment-6456123781).

Frozen contributor head: `c89470c442d28195bebba45597910c7a481a5043`.
Qualified published head: `9868a224b12b511eada6bdd4c9fa5a718da9258b`
(history-preserving merge of then-current master into
`FabioMalpezzi:pr/js-arrow-after-a-member-access`; maintainerCanModify).

## Claim

Member-assignment arrows after `.` must report their parameters; ordinary
member calls `a.b(x)` remain calls. On target `aa5f406` before correction,
`analyze_file` reported `module.exports.add` param=0 and `load` param=1
(expected 2 each). Corrected tree reports 2/2.

## Correction

Adapted frozen PR onto current target in
`lizard_languages/typescript_states.py` (stop sticky `_prev_token` for `.`
beyond the next token; keep `new`). Contributor tests in
`test/test_languages/testES6.py`. Product-edit time ≪15 minutes.
ADR-0000 / ADR-0001; no parallel counters. Post-change refactor: none —
already clean.

## Focused / full proof

- Contributor tests + focused JS/TS suite (611 passed / 5 skipped)
- Full `pytest`: 2129 passed, 7 skipped
- `make`: exit 0
- Required CI on exact head: python-app matrix + `changes` pass; `web` skipped (not applicable)

## Performance

Six paired warmed workloads (20 pairs). Incremental api / api-parameters / cli
and cumulative api / api-parameters passed at 1.2s. Cumulative cli at 1.2s was
inconclusive (upper CI 4.58%; retained as
`cumulative-cli-inconclusive-1.2s.json`); prospective 4.0s retry passed
(upper CI 2.11%). See `measurement-decision.json` and result JSON files.
