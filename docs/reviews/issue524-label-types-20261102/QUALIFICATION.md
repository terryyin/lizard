# Issue 524 parser qualification

Runtime labels retain callbacks and successors. Type annotations own their nested syntax and return enclosing runtime delimiters. Typed fields, contextual identifiers, optional punctuation, templates and generic/import/query types preserve actual runtime functions and metrics.

Local validation: full pytest and make both exit0; **2917 passed,7 existing skips**. Two regression modules exercise490 actual fixture contexts; independent aggregate refactoring is complete.

Frozen workloads:35 label sources,432 annotation sources,40 unchanged call consumers,638 complete sources and100 unchanged broad CLI sources. Starting master: da29d5dfd904ba7333199a79ffbf4c0da24c5641; cumulative baseline:6d288d4cdcb85bc50ab4c8a887196177607eeb7d.

Installed paired runner; API/parameter/NS calibration1.2s, CLI4s, every batch>=1s. Call families use40 alternating pairs; other cells20. Current calls/NS uses a prospectively fixed80-pair four-second extension pooled with all original40; both extra80 and pooled120 pass. All raw failures/pairs remain retained.

| Comparison | Pairs | Paired95% upper slowdown |
| --- | ---: | ---: |
| current-calls-api-parameters | 40 | +2.7992% |
| current-labels-api-parameters | 20 | -1.6282% |
| current-annotations-api-parameters | 20 | +0.2550% |
| current-complete-api-parameters | 20 | +1.4180% |
| current-calls-ns | 120 | +0.8488% |
| current-labels-ns | 20 | -1.1816% |
| current-annotations-ns | 20 | +2.0288% |
| current-complete-ns | 20 | -2.0335% |
| current-broad-cli | 20 | +0.9436% |
| cumulative-calls-api-parameters | 40 | +0.5651% |
| cumulative-labels-api-parameters | 20 | -2.0019% |
| cumulative-annotations-api-parameters | 20 | -0.5902% |
| cumulative-complete-api-parameters | 20 | -2.7395% |
| cumulative-calls-ns | 40 | +1.2666% |
| cumulative-labels-ns | 20 | -0.1838% |
| cumulative-annotations-ns | 20 | +1.2100% |
| cumulative-complete-ns | 20 | -1.7658% |
| cumulative-broad-cli | 20 | +1.6705% |

Maximum accepted upper: **2.7992%**, strictly below3%. Current108 runtime-file hashes match full local checks and independent audit; source/corpus/harness/import/observation stability is retained for every comparison.

Detailed working evidence remains in the owned execution checkout through hosted disposition and will be preserved in Git before ordinary wrap-up cleanup. The repair increment contains the parser, regressions and this concise qualification; hosted exact-head checks are still required before merge.
