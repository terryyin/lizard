---
id: SEED-069
status: active
planted: 2026-11-20
planted_during: SEED-066 wrap-up
scope: reduce the ND/NS cost of finding implied semicolons in JavaScript/TypeScript
---

# SEED-069: Cheaper implied-semicolon scan for ND/NS

<a id="shared-implied-semicolons-scan"></a>

### Cheaper implied-semicolon scan for ND/NS

**Identity:** SEED-069#shared-implied-semicolons-scan
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/034-cheaper-implied-semicolon-scan/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"d2ab16a0dcee605f0415147ff7172f7382ef3756d2f17de52924b8bf53b69088","plan":"2b6e8e12ede77fc86130a0c64ee0044d758f8201665dc68f4561d836a18fadd4"}}
```

**Goal:** Reduce the cost of finding implied semicolons for users analyzing JavaScript and TypeScript with Nesting Depth (`-END`) or Nested Structures (`-ENS`), while preserving the results delivered by #578. Each extension should be at least **3%** faster than #578 on the frozen mixed workload (paired API median ≤ −3%, or clear the stock 3% paired harness as faster than #578) and within 15% of the pre-#578 runtime. *(Amended 2026-11-21: developer chose to lower the original 5% bar after call-seam delivery measured ~−2.9% ND.)*

**Scope:** Lower the work done by `ImpliedSemicolons` or its ND and NS consumers when either extension is selected alone or both are selected. Preserve the statement boundaries they consume and the ordinary analysis results. Keep analysis without either extension within the existing 3% performance screen. Metric definitions and additional automatic semicolon insertion cases, including the parenthesized assignment in [SEED-068](SEED-068-js-asi-parenthesized-assignment.md#js-asi-parenthesized-assignment), are separate work.

**Key examples:**

- A JavaScript or TypeScript function with consecutive braceless `if`s and no semicolons has ND 1 and NS 1. A braceless `if` inside another braceless `if` still has ND 2 and NS 2.
- Analyzing the original SEED-066 frozen JS and mixed corpora before and after the change gives identical function identities, locations, and nesting values for each selected extension, including when both run together.
- On the frozen mixed workload, each extension passes the paired median targets on its own using 40 pairs calibrated to 2-second batches. The frozen JS workload also passes the plain analysis screen.

**Source:** SEED-066 approved #578 under a 25% paired median ceiling on 2026-11-20. The historical results were +21.8% for ND and +21.3% for NS. The 15% target aims to recover about a third of that overhead. The earlier attempt to gate `before()` on tracked tokens and line changes gave no measurable gain.
