# PR #554 source-review commitments

Story obligations for SEED-034#process-fabio-malpezzi-prs / slice 4.

| ID | Observation |
| --- | --- |
| E4 | Candidate `_member_name` after `.`/`?.` makes `-ENS` treat `.catch`/`?.catch` as non-structures while real `if`/`try`/`catch` still raise NS (`reproduction.json`). Left unmerged: cumulative NS `api-parameters` gate fail (attempt5). Trunk still exhibits the defect. |
| 1 | Value: accurate NS for promise/member names that collide with structure keywords. |
| 2.2 | Owner remains `lizard_ext/lizardns.py`; no parallel counter. |
| 9.1 | Discriminating regression present and green on candidate. |
| 9.2 | Full pytest + make + required CI green on exact head. |
| 12 | Independent refactor: none warranted (as-authored patch sufficient for semantics). |
| 16 | Performance: incremental api/api-parameters/cli pass under `--extension ns`; cumulative api pass; cumulative api-parameters fail (decisive); cumulative cli incomplete under load — insufficient for merge regardless. |
| disposition | Non-merge + SEED-035 follow-up per REVIEW-CONTRACT ≤15-min / performance protocol (same pattern as prior #545). |
