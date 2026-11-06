# PR #554 — Member names that look like structures under NS

**Disposition:** SEED-035 cleared every required quiet-load paired `--extension ns`
gate (including cumulative `api-parameters` and CLI) for the as-authored
`_member_name` candidate on refreshed master; story-branch wrap-up lands the
equivalent exact-head revision and acknowledges the contributor. Attempt5
evidence below remains the SEED-034 non-merge record.

**Head:** `2368413b2de05b05ff44ce863dcded8061342492` (OPEN; hosted CI green).
**Incremental target:** `ed78b3b13240297681f82fe6a32c84c377498114` (after #553).
**Frozen cumulative baseline:** `4a05e4056cebb2edd75cb02b7a43e911369f4ce5`.

## Value

Under `-ENS`, `.catch` / `?.catch` member calls inflate `max_nested_structures`.
Correct behavior skips structure keywords that appear as member names while
real `if` / `try`/`catch` still raise NS. Accurate nesting benefits API users
and thresholds. Owner: existing `lizard_ext/lizardns.py` state machine.

## Adaptation

Contributor patch applies as-is: `_member_name` after `.` / `?.`, plus
`test_member_named_like_a_structure_is_not_a_structure`. No correction required
within the 15-minute product budget; independent refactor: none warranted.

## Proof (semantics / suites)

- Target red / corrected green: `reproduction.json`.
- Focused + full pytest + make: pass on candidate.
- Hosted CI: all required builds pass on the exact head.

## Performance

Attempts 1–4 retained (noise / timeouts under host contention). Attempt5 on a
quiet machine (load after ~2.4):

| Comparison | Workload | Median | Paired 95% CI | Gate |
| --- | --- | ---: | --- | --- |
| incremental | api | +1.221% | [−0.051%, +2.508%] | pass |
| incremental | api-parameters | −1.435% | [−2.224%, −0.639%] | pass |
| incremental | cli | +0.288% | [−1.099%, +1.694%] | pass |
| cumulative | api | +2.006% | [+1.450%, +2.566%] | pass |
| cumulative | api-parameters | +3.638% | [+2.770%, +4.513%] | **fail** |
| cumulative | cli | — | — | incomplete (timeouts under later load) |

Incremental `#554` alone is within budget (api-parameters even slightly faster).
The cumulative `api-parameters` overage under `--extension ns` is therefore
dominated by the already-accepted set (#551–#553 were not measured with `-ENS`).
No ≤15-minute product change to this PR can spend that budget down. Further
timing of the same candidate seeking a pass is refused by contract.

## Follow-up

See SEED-035. Beneficiary: `-ENS` users of promise/member `catch`. Source PR
and evidence root: this directory / `#554`.
