# Source-only review

**Story:** SEED-069#shared-implied-semicolons-scan  
**Launch:** Fresh general-purpose reviewer after 2026-11-21 goal amendment (3% bar); Goal, Scope, key examples, planting notes only.  
**Prior review:** replaced (5% bar).  

## Unnumbered source context

SEED-069 aims to cut the cost of implied-semicolon work for Nesting Depth (`-END`) and Nested Structures (`-ENS`) on JS/TS, without changing what #578 delivered. Work targets `ImpliedSemicolons` or its ND/NS consumers when either or both extensions are on. Metric definitions and further ASI cases (including SEED-068 parenthesized assignment) are out of scope. Historical note: #578 was approved under a 25% ceiling (~+21.8% ND / +21.3% NS); an earlier `before()` gate did not help; the bar was lowered from 5% to 3% after ~−2.9% ND on call-seam delivery.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| C1 | ND ≥3% faster than #578 (mixed): Each extension should be at least 3% faster than #578 on the frozen mixed workload (paired API median ≤ −3%, or clear the stock 3% paired harness as faster than #578); 40 pairs × 2s batches — for Nesting Depth alone. | slice 4 |  |
| C2 | NS ≥3% faster than #578 (mixed): same targets as C1, for Nested Structures alone. | slice 4 |  |
| C3 | Within 15% of pre-#578 runtime: within 15% of the pre-#578 runtime (each extension on that workload). | slice 4 |  |
| C4 | Preserve #578 / ordinary results: preserving the results delivered by #578 / Preserve the statement boundaries they consume and the ordinary analysis results. | slice 4 |  |
| C5 | Braceless consecutive ifs → ND 1, NS 1. | slice 4 |  |
| C6 | Nested braceless if → ND 2, NS 2. | slice 4 |  |
| C7 | Frozen corpora unchanged for ND / NS / both: identical function identities, locations, and nesting values for each selected extension, including when both run together. | slice 4 |  |
| C8 | Plain analysis stays within 3% screen on frozen JS workload. | slice 4 |  |
