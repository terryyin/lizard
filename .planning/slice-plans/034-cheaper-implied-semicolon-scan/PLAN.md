# Cheaper implied-semicolon scan for ND/NS

**Identity:** SEED-069#shared-implied-semicolons-scan

**Source:** [Cheaper implied-semicolon scan for ND/NS](../../seeds/SEED-069-shared-implied-semicolons-scan.md#shared-implied-semicolons-scan)

## Goal and scope

Reduce the cost of `-END` and `-ENS` for JavaScript and TypeScript while preserving the statement boundaries and nesting results delivered by #578. On the frozen mixed workload, each extension must be at least 5% faster than #578 and within 15% of the runtime immediately before #578. Plain analysis retains its 3% screen. The seed owns the examples and the scope of automatic semicolon insertion covered here.

## Existing solution and decisions

`lizard_languages/javascript_statements.py` owns the boundary rule. `TypeScriptReader.implied_semicolons` also supplies it to JavaScript, JSX, and TSX. The two extensions take different actions on the same boundary. ND closes its current statement when the condition depth permits it. NS presents a semicolon to its own state machine. Each currently drives a fresh scanner through the token stream after line counting. Preserve the token text, line numbers, and delivery order that feed both extensions and the language reader.

Start with the scanner's state transitions for common tokens. Use the retained profile to choose one cohesive change in `before()` and `_read()` that reduces state work shared by both extensions. Inspect group transitions and pending head, body, label, and loop state together because their ordering affects when a statement ends. Keep a single implementation of the boundary rule and verify the exceptional paths with the existing #578 fixtures. The earlier gate around `before()` gave no measurable gain. Sharing a scanner only when both extensions are enabled cannot meet the single-extension timings. The reader's other expression-boundary checks use their own parsing context and need no changes for this route.

The straight-through case edits `javascript_statements.py` (184 lines at `916d8193`). The existing extension callers and oversized fixture files can supply proof without edits. Apply the 250-line file check if the implementation seam changes. Use metric and language-reader names from [ADR-0001](../../../docs/adrs/0001-ubiquitous-language-accepted.md) and follow decision ownership under [ADR-0000](../../../docs/adrs/0000-use-adrs-accepted.md).

## Outside-in proof

| Promise | Slice 1 observation |
| --- | --- |
| Preserve implied ends and true nesting | Run `nix develop -c python -m pytest -q test/testNestingDepth.py test/testJavaScriptNestedStructures.py test/testNestedStructures.py test/testNestedStructureScopes.py test/testPythonNestedStructures.py`. These include sibling and nested braceless controls, multiline expressions, blocks versus objects, template/JSX forms, and TypeScript assertions. Also run the sibling witness via `check578.py` through `FileAnalyzer` with both extensions. |
| Preserve function identities, positions, and nesting values on both frozen corpora | With `eq.py`, compare rows across `022f6200` and the candidate for each of `nd`, `ns`, `nd,ns`, and `ns,nd`, using the original `.js` filenames and replaying the same contents with `.ts` filenames to exercise the TypeScript reader. Compare filename, function name, start and end, CCN, ND, and NS. Exercise JSX and TSX with the existing fixtures. |
| Reduce ND and NS runtime independently | Run each on the mixed corpus against both pinned revisions. Require a paired median at most −5% against `022f6200` and at most +15% against `c214bb46`. Retain paired rows and intervals for both extensions. |
| Preserve ordinary analysis and consumers | Compare plain API observation digests against `022f6200` on the frozen JS corpus and require the stock 3% paired screen. Run `nix develop -c python -m pytest` before completing under `AGENTS.md`. |

The planning trace searched `ImpliedSemicolons`, `implied_semicolons`, `max_nesting_depth`, and `max_nested_structures` at `916d8193`. Rerun the installed consumer trace on the candidate. The scanner and the two extension paths are exercised through the real `FileAnalyzer`. The option thresholds and output formatters read the same metric fields and are covered by corpus parity and the full suite, including `test_options.py`, `testExtension.py`, and `testOutput.py`. The full suite also covers direct metric readers in JavaScript binding, TypeScript annotation, optional operator, cognitive complexity, PHP, Python, and ST tests. Historical done records and prompt history do not consume live metric values.

## Proof inputs and observations

Use `~/.open-dough/evidence/lizard/SEED-069` for the two corpora, manifests, SEED-066's `check578.py` and `eq.py`, and the preparation replay. The corpora match the original SEED-066 job copies byte for byte. `corpus-js` contains 18 `.js` files and its manifest SHA-256 is `c8506ceb54f39f4b4cc81b53f95346c302f2002efb451135cde3e94708e2b640`. `corpus-mixed` contains 40 `.js` files and its manifest SHA-256 is `7f90da117dffb99441108c6716d0454839b4d8bc7dbef0f165fa9fec056fb773`. Both manifests hash paths and file contents. Use the unchanged names for timings to match SEED-066. The `.ts` replay is a correctness check.

For isolated timings, use detached source trees for `022f62007c48784900102f1ef196b818489f9823` (published #578, identical production bytes to the preparation base) and its first parent `c214bb46689540a8ab39388563d6c1f47e0b645e` (trunk just before #578). Historical SEED-066 q2 timings used the earlier `cb74bd63` baseline before #576/#577 landed. Use fresh pairs on the pinned comparisons for acceptance.

```sh
nix develop -c python .agents/skills/lizard-process-pr/scripts/paired_benchmark.py \
  --baseline "$BASE" --candidate "$PWD" --corpus "$EVIDENCE/corpus-mixed" \
  --out "$EVIDENCE/$RUN.json" --workload api --extension "$EXTENSION" \
  --pairs 40 --seconds 2
```

Run once per base and extension (`nd` and `ns`). For the plain control use `022f6200`, `corpus-js`, and omit `--extension`. Keep outputs outside the corpora and check that reports contain complete rows, valid batches, isolated workers, matching input manifests, and stable observations. For ND and NS, evaluate `100 * (median(candidate_seconds / baseline_seconds) - 1)` from `raw`. The harness's default verdict uses the upper confidence limit of a log-mean ratio. Its API digest omits ND and NS, so the explicit `eq.py` comparison carries nesting proof. Use the default harness verdict for the plain control.

Check host load and competing work immediately before timing. On Territory.local during preparation, load was about 36/23/17 on 16 logical CPUs, so preparation used worker smoke and count profiling only. Run the full screens sequentially on a quiet host and retain a same-revision no-op for a noisy run. If noise overlaps the decision, gather one fresh set on a quieter host before accepting a result. Five screens at 40 pairs and roughly 2 seconds per batch require at least 13 minutes of command time plus calibration. Capture baseline and candidate with the same interpreter and options per pair.

| Premise | Preparation observation at `916d8193` |
| --- | --- |
| The sibling witness distinguishes the #578 behavior | `check578.py` with the Nix interpreter and both extensions gave ND/NS 3/3 from the `c214bb46` source tree and 1/1 from the preparation checkout in both `.js` and `.ts`. CCN stayed 4. The focused suites returned 128 passed and 2 skipped. |
| The parity routes run through the selected readers and report the affected metrics | The `eq.py` replay gave 4,480 matching function rows per suffix across the selected extensions and both orders, with each extension's field compared where present. The preparation replay exercised `.js`, `.ts`, `.jsx`, and `.tsx`. |
| The paired API uses the pinned source trees and frozen workloads | `prepare-observe.py` invoked the actual isolated measurement worker with `--workload api`'s worker spec for ND, NS, and plain on `c214bb46` and `916d8193`. It processed 40, 40, and 18 files respectively, each with a positive sample and stable observation digest. The harness checked imported module locations. Full paired timings belong to Slice 1. |
| Scanner-local work reaches both single-extension runs | The same replay profiled the 40-file mixed workload through `FileAnalyzer`. Each single-extension run called `before()` 52,400 times, `_read()` 21,000 times, and `_statement_ends()` 7,000 times. Both extensions together doubled those counts. Plain analysis made zero calls to the scanner. |

Replay the retained observations with `nix develop -c python "$EVIDENCE/prepare-observe.py" "$PWD" "$PRE_578" "$EVIDENCE"` from the preparation checkout, with `PRE_578` pointing to an exported `c214bb46` tree. The retained observation log is `preparation-observations.txt`.

## Slices

### 1. Reduce statement-boundary scan cost for ND and NS

Type: Behavior
Status: planned
Proof: Paired API medians, standalone and combined metric parity, #578 regression fixtures, and the required full suite above.

Behavior: Given a JavaScript-family file analyzed with ND, NS, or both, Lizard reports the same nesting and ordinary metrics as #578. The same frozen mixed input analyzed with either extension alone meets both runtime targets. Plain analysis stays within its prior screen.

Start from the retained profile, make one localized change to the scanner's common transitions, and rerun the mapped proof. If the first coherent change misses the bound, retain the measurement and use it to reassess the design before expanding into reader or extension plumbing. The 15 active minutes cover the localized edit, review, and proof inspection. The timed commands run through the slice clock. No prep-only production structure is needed.

## Plan review

Slice-plan refinement was unnecessary because the same scanner change serves both independent extension selections and the proof is owned by one bounded Behavior slice. The recorded measurements qualify the proof route. The candidate's speedup is the slice outcome to verify.
