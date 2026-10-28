# What the PR history taught us

Research date: 28 October 2026. This synthesis supports the processing skill; it
does not replace current story contracts or certify an active PR's state.

## Evidence recovered and limits

- Repository Git history, including merged PRs and removed story/plan records.
- The retained `docs/reviews/fabio-malpezzi-20261027/` cohort: 19 frozen PRs,
  report/authored examples, original and corrected proposals, consumer traces,
  literal harnesses, source/corpus manifests, raw timings and dispositions.
- All 75 available main Cursor transcript files under the local Lizard project
  directories were inventoried. Focused excerpts were read for PR review work,
  including #485, #486, #488 and #517, and recent refinement/execution sessions.
  Thirty-two files mention PR/issue numbers in user records; that is an index
  count, not 32 independently verified PR completions.
- All 23 available main Claude Code transcript files under Lizard project
  directories were inventoried. Focused records include SEED-014 execution
  (`315328aa-e510-44b5-a987-a6bebfa16ea6`) and SEED-015 refinement
  (`d0293408-fa07-482e-800d-a7987d414400`). Injected skill text and tool results
  were distinguished from human instructions.
- Codex's “Review Lizard pull request 518” (`01a14e3e-5534-7da0-a096-2a0f3a977d39`)
  supplied the original review, correction, CI/merge, cleanup and acknowledgement
  instructions. The Fabio refinement chat (`01a17b6d-84f7-73c3-97c1-142664409c97`)
  retained the ADR-status conflict and the human's later acceptance.
- The Codex chat “done-Lizard · Execution · Review, correct and merge
  FabioMalpezzi's open PRs” (`01a17b82-00e1-78a3-90d5-d61ed25c82b2`) supplied
  the human request to split the large batch into a few independent full cycles.
  Done records corroborate host ownership: SEED-010/015/016 Codex, SEED-013
  Cursor, SEED-014 Claude. A host label establishes attribution, not relative
  agent quality.
- Live GitHub metadata and discussions were checked for #512, #517, #518, #527,
  #531, #533, #534 and #536. Claims below use their retained evidence as well.

This is the accessible local history, not a complete archive of every cloud chat,
deleted session or contributor environment. No cross-host token/elapsed-time
benchmark was available. Fewer printed payloads and reusable commands should reduce
repeated work; no numerical saving is claimed. Active structural/lexical batches
were still producing commits during research. Their old “queued” mapping is not
used as current status and partial transcripts are not treated as completed proof.

## Repeated work and the resulting practice

| Repeated action or observed failure | Evidence | Practice retained |
| --- | --- | --- |
| The same review request repeatedly asks for problem correctness, code correctness, code smells and speed; later requests separately ask for fixes and aggregate refactoring. | Cursor sessions `fb82e7f2` (#485), `cc505a23` (#486), `5b2d37e6` (#488), `5945e297` (#517). | Put the entire requested cycle in one backlog owner; inspect PR plus corrections as one conceptual change. Preserve review-only scope when that is all that was asked. |
| An example is fixed but nearby grammar forms, later functions or public metrics regress. | [#517 review](https://github.com/terryyin/lizard/pull/517#issuecomment-6102793124); DD-010 in `DearDough.md` records two gaps caught by coordinator probes after green tests. | Reproduce target and candidate through public entry points; add a small ambiguity/control set before accepting parser changes. |
| A local fix bypasses an existing semantic owner. | [#536 acknowledgement](https://github.com/terryyin/lizard/pull/536#issuecomment-6447350182); its original second name collector disagreed with named arrows and missed empty parameter slots. | Trace names/counts and downstream consumers; put formal parameters in the shared signature/FunctionInfo owners. |
| Correct code adds cost to every ordinary token. | [#518 corrections](https://github.com/terryyin/lizard/pull/518#issuecomment-6250240662); [#533 corrections](https://github.com/terryyin/lizard/pull/533#issuecomment-6443273326); #534 optimization commits `13c633b`, `737aeaf`. | Inspect ordinary paths as well as rare syntax. Profile a valid candidate when a gate fails; remove redundant wrappers, probes and snapshots at their owner. |
| Broad timing looks fine while the affected consumer is slower. | Original #536 raw proposal: API interval +3.917% to +5.428%, names/counts +3.443% to +4.956%, broad CLI -2.568% to +1.200%. See its retained README/results. | Every required workload passes independently. A broad result cannot cancel a local regression. These are historical proposal results, not the subsequently merged correction. |
| A benchmark was omitted until after merge. | DD-004: #527 merge `e871ae4` preceded expanded affected nesting controls. Later measurement passed but did not repair ordering. | Freeze the complete workload list before timing and complete each applicable gate before merge. |
| Large/noisy runs invite repeated timing and discussion. | Codex batch chat retained an inconclusive CLI gate under host load; #536 chose longer CLI batches prospectively using #535's noise evidence. | Check load, serialize owned heavy work, calibrate long batches, alternate pairs, retain all raw observations and stop with an inconclusive result instead of rerunning until green. |
| Each PR recreates collection, snapshot, integrity and timing code. | `pr528/`, `pr531/`, `pr533/`, `pr536/` contain similarly shaped reproduction/profile/bootstrap/finish scripts; central `paired.py` already owns paired math. | A read-only packet collector and a reusable isolated paired runner now handle the mechanical parts. Reproducers and workload selection remain specific to the behavior. Historical harnesses stay immutable. |
| Import probes observe the wrong checkout. | DD-006: `python -c` gives Lizard an argv0 directory in the caller's checkout; old root `profile.py` can also shadow stdlib profiling. | Real worker scripts outside the snapshot, isolated cwd, asserted module paths before/after, one interpreter and hashed inputs. Import profiling modules before inserting a legacy snapshot. |
| Read-only commands still race in setup and flood output. | DD-003 recurs across all three hosts; `shell.nix` recreates `.venv` and runs pip for each Nix entry. | Serialize Nix entries per checkout; batch independent reads in one entry and retain logs with compact summaries. Do not change the environment incidentally while reviewing a PR. |
| Permission signals or no-op probes incorrectly reject a contributor route. | DD-008, PR528 `maintainer_can_modify: true`, fork `push: false`, divergent dry-run accepted. | Test the actual non-mutating route and preserve contributor commits. Discover inability to publish before doing a large correction. |
| Delivery stops on approval classification despite recorded task authority. | DD-009, Claude SEED-014. | Carry existing human authorization into execution and retain it on resume; a skill itself grants no new external-write permission. Explain actual host rejection when it happens. Do not routinely ask again. |
| Clean native Git output hides domain reconciliation obligations. | DD-005: contributor and validation merges bypassed the backlog adapter; no corrupt result was observed. | Resolve the installed backlog Git adapter before merges touching planning/done records, including clean merges. |
| Integrity checks reject acceptance notes or include their own output. | DD-011, #534. | Freeze measured product/corpus/harness inputs separately from evolving proof records. Exclude output from its own manifest. |
| Reconstructed CLI output hashes differ only because paths moved. | DD-012, #534. | Retain literal arguments and actual output; explain old/new path correspondence without normalizing away semantic differences. |
| Discovery and CI responses consume huge context. | DD-013 records 23,070-token check output, a 20,788-token tool discovery and 125,371-token installation search. | Exact-name tool discovery, targeted source reads, raw packets on disk and compact projections of job ID/name/state/conclusion/SHA/URL. Truncation never establishes completeness. |

## How the process evolved

Earlier Cursor work often followed “review → human merges → correct → refactor
again.” This was explicitly requested in those sessions, not agent misconduct.
It exposed the repeated need to consider the contributor patch and local fixes
together. #517's follow-up commits `4af401c` and `cfbcc88` are an example.

Later work integrated correction before merge. [#512's final comment](https://github.com/terryyin/lizard/pull/512#issuecomment-6328890814)
records corrected Go edge cases, aggregate refactoring and alternating performance
rounds. #518 removed per-token adapters and checked extension/token semantics.
Its chat shows review, correction, merge and posting were separately authorized;
an incoming PR link alone never supplied permission for every outward action.
The larger Fabio cohort then made exact revisions, paired uncertainty bounds,
incremental/cumulative workloads, contributor publication and acknowledgement
explicit. Its [complete-cycle contract](../../../../docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md)
is the source of its strict <3% upper bound and >15-minute correction deferral.
These historical policies are not evidence that every earlier PR met them.

The first eight review dispositions produced four larger correction stories
(SEED-013/014/015/016), subsequently merged across Cursor, Claude and Codex.
The owner explicitly rejected making later stories mere QA phases: each batch
must conclude its own PRs. Existing batch ownership remains valid; new standalone
PRs enter the backlog individually unless deliberately grouped into a bounded story.

## Automation boundary and remaining opportunities

Implemented here: paginated read-only PR packets with integrity checks and concise
summaries; isolated API/public-parameter/CLI workers; calibrated alternating paired
timing, immutable input checks and strict confidence-bound evaluation.

Reused: product-backlog identity/allocation/mutation, story lifecycle, refactoring,
CI observation, publication and merge adapters. A second implementation of those
would create drift and consume more maintenance/context.

Human/agent judgment remains necessary for expected semantics, value, affected
consumers, scope/estimates, noise interpretation and proof applicability. Neither
helper approves or merges a PR. Future improvements could make Nix setup quiet and
idempotent and improve observer identity receipts (DD-001); those are separate
maintenance concerns and are not changed by this skill.
