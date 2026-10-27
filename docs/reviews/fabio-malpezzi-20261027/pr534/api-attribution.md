# Attribute remaining current-target API cost

Exact candidate `ec2ec81565e037883066f220ab794f912b8df2b8` versus current
`34cd43ddd1915438f3b440088672bfe4a08d96bf`. The six statistical observations
in [final-performance.md](final-performance.md) remain unchanged: current API
upper bounds +6.891% original and +7.259% augmented fail materially. Both CLI
and both frozen API comparisons pass. No quiet retry is justified by these
intervals; the original 128 gate remains independent of the augmented 166 gate.

## Bounded decision and actual workload

The question is which avoidable work remains, separating necessary additional
function reports from orchestration or duplicate recognition. Territory.local
had 16 CPUs/48 GiB, load 8.72/7.99/7.71 and about 13.6 GiB inactive memory.
Native VM activity is outside this execution's ownership. No other owned heavy
proof was active; one serial diagnostic core with modest memory is credible.
Inspect again before repeats or changed contention. This is attribution, not
another qualification run.

Four serial profiles use 100 corpus passes each, current/candidate × original
128/augmented 166. Inside the profile loop, only public analyze_source_code is
called, exactly as the immutable API worker does; seven-field extraction is
outside that loop. Warm five passes first, assert imports and immutable source
hashes, then compare all fields before/after within each snapshot. A separate
observational pass counts successful/false probes without changing their code.

```sh
PIP_QUIET=1 nix develop -c python /private/tmp/lizard-seed015-ec2-api-profile.py
```

The command and all four worker processes terminated 0. Its exact source is
retained as api-attribution-profile.py. Raw profiles, pstats, logs, receipts and
host/decision observations are in
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-ec2-attribution`.
api-attribution-summary.json retains their hashes and selected actual rows.

| Signal | Original 128 | Augmented 166 |
| --- | ---: | ---: |
| Current profile seconds | 4.9134 | 6.5290 |
| Candidate profile seconds | 5.0998 | 6.8203 |
| Successful / false probes | 24 / 25 | 30 / 57 |
| Current / candidate foreground annotation calls | 333 / 318 | 396 / 363 |
| Candidate generic reader own seconds | 0.2199 | 0.2920 |
| Candidate no-op process_token own seconds | 0.0265 | 0.0356 |

Correct splitting adds 81/87 reported functions per corpus: try/push/model
calls increase by 8,100/8,700 across 100 passes. That is required work, not
duplicate recognition. Helper cumulative times 0.1089/0.1919 seconds include
advancing the original tokenizer; they cannot all be attributed to new grammar.
Unchanged constructors also show own-time variation. No complete causal
allocation or predicted throughput gain is claimed from these single profiles.

## Equal-output control and existing completed owner

One decisive follow-up checks whether common bookkeeping remains when no
new function or annotation probe occurs. Fixed controls: block_branches.js/ts,
plain_ternary.js and internal_ternary.js, 1,000 passes/version. Both versions
have identical function/state call counts, no return_type_colon call, and all
seven public fields are exactly equal. Profile totals are 1.0621/1.0894 seconds;
new preprocess/global-state checks are small, with unrelated timing variance.
This does not establish a statistical slowdown or assign all cost to metadata.

The initial control setup imported the candidate-only parameter_group helper
in current and exited 1 before profiling. Its exact source, log and receipt
remain retained. Control2 imports it only for the candidate observational pass;
no product code changed. Both workers and the corrected driver terminated 0:

```sh
PIP_QUIET=1 nix develop -c python /private/tmp/lizard-seed015-ec2-control2-profile.py
```

The exact corrected source is api-attribution-controls.py. A separate single
166-input observation found 107 accepted contexts. Every one has the canonical
pending summary and binding owner; binding.at_start matches actual group
emptiness, captured opening-prefix eligibility matches, and no callback or
missing-owner exception occurred. Empty full_parameters is not a substitute
for true token emptiness. These observations support an existing-owner option,
not an invariant for every unobserved accepted form.

## Simpler-choice comparison and selected remaining work

Read-only PFE inspected the whole product's token-role/span/countdown owners,
current Go/TS callers and Accepted ADR-0000/0001. Its actual return is retained
in the parent execution conversation. Go's signature receipt can convey a
confirmed header while counters see every original token once. TypeScript's
canonical type owner has both saved-runtime-token and own-closing-bracket
completion forms; a receipt must distinguish them. It would help only successful
ambiguous probes, not direct typed/empty groups or false probes. Current data
does not justify introducing that machinery as a sufficient budget remedy.

Canonical BindingSummary could replace marginal completed-group bookkeeping,
preserving true emptiness, prefix eligibility and typed-anywhere across commas.
That is a smaller owner than a new receipt, but its measured marginal work is
less than reader dispatch. Do not broaden both representations speculatively.

Select local TypeScriptReader single-state/default-hook delivery, following the
existing GoReader delivery pattern. The actual JS/TS/TSX/Vue readers have one
state and no process_token override; generic reader own/no-op work is materially
larger than marginal metadata. Replacement still iterates and dispatches, so
the whole measured total is not a savings prediction. Preserve inherited and
instance hook overrides, multiple/no-state generic fallback, errors, original
token/counter delivery, state finalization and eof. Change no grammar, parser,
counter or raw-source filter; no Go grammar dependency or new architecture.

Refine unfinished measurement 7 into this Structure 7, retain exact-revision
six-gate qualification as 8 and unchanged contributor delivery as 9. Completed
1–6 and all evidence remain. Nine cohesive slices fit the supplied unconstrained
sizing; no story resplit or human scope decision is needed. Existing PFE/ADRs
still support local reader ownership. Reassess readiness through the shared
recorder, then delegate fresh implementation/refactoring before measurement.
All six future bounds remain strictly below 3%, with no averaging or waiver.
