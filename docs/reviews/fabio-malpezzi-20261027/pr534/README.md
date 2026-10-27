# PR534 correction and retained evidence

[PR534](https://github.com/terryyin/lizard/pull/534) is merged as
[48de7f2](https://github.com/terryyin/lizard/commit/48de7f294831b0b098c9ab2753c831346c645fd8)
and [acknowledged](https://github.com/terryyin/lizard/pull/534#issuecomment-6451652906).
The qualified contributor head0bfc798 preserves authored f4074ae and current
signature/binding owners. The merge tree equals that qualified head.
Current behavior and reader contracts are documented in
[arrow boundaries](../../../javascript-arrow-boundaries.md); regression tests
remain in the product.

The complete source, ten-slice plan, proof decisions, raw-result copies,
immutable manifests, literal scripts and hosted/landing receipts are recoverable
at [before-cleanup e01d2d90](https://github.com/terryyin/lizard/tree/e01d2d90cc8bdbc6eec8a74043f858986d3757cd/docs/reviews/fabio-malpezzi-20261027/pr534).
The completed plan is at the same revision under
`.planning/slice-plans/011-ternary-arrow-boundaries/PLAN.md`;
the source is `.planning/seeds/SEED-015-ternary-arrow-boundaries.md`.
Recover a file with `git show e01d2d90cc8bdbc6eec8a74043f858986d3757cd:<path>`.
The active snapshot retains shared cohort inputs and later review-batch owners.

## Final proof

Exact measured code737aeaf is byte-identical to contributor head0bfc798 across
all104 product files and159 test files. All166 public/API/FileAnalyzer rows and
seven fields pass; the broader CLI output is byte-identical. Full pytest and
make each passed2118 tests/7 skips; make coverage94%, pycodestyle0. Configured
pylint completed with its retained exit-zero diagnostics.

Before the exact-head hosted merge, both
[origin](https://github.com/terryyin/lizard/actions/runs/40569352172) and
[PR](https://github.com/terryyin/lizard/actions/runs/40569487477) runs passed all14
Ubuntu24.04/Windows×Python3.8–3.14 builds plus changes; web was inapplicable.
Execution completiondb1b24 also has exact nativeCI success
[40572141303](https://github.com/terryyin/lizard/actions/runs/40572141303) and
confirmed observer shutdown. Later closure CI is a separate lifecycle.

All six paired95% upper slowdown bounds are strictly below3%:

| Workload | Current34cd43d | Frozen2e21e404 |
| --- | ---: | ---: |
| Original128 affected API |1.7136% |-8.1846% |
| Broad100 CLI |0.3111% |0.6326% |
| Augmented166 affected API |2.5280% |-4.2216% |

Each comparison ran once with20 alternating warmed pairs,1.5-second calibration,
all batches>1second, Nix Python3.9.11 and immutable inputs. The original92 plus
four supplemental and32 signature cases remain the independent128 workload;
all38 added query/trivia controls also comprise the separate166 workload.
Earlier failed corrected candidates and setup/diagnostic mistakes remain in
recoverable history; raw-proposal timing was unproved, not an acceptance result.

The archived root-freeze.json fixes candidate737aeaf, current34cd43d and frozen
2e21e404. Original harness SHA256 is3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5.
Recover the complete PR534 directory from e01d2d90 to replay literal freeze,
verification and measurement scripts; keep original/corrected protocols and
exact source/corpus hashes distinct. The shared parent README retains the
historical100-file corpus reconstruction recipe and original harness revision.
Process findings remain in DearDough.md; no new correction was recommended.
