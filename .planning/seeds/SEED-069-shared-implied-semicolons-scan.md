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

**Goal:** `-END` and `-ENS` on JavaScript/TypeScript keep the #578 no-semicolon results while costing well below the current ~+22% paired median over the pre-#578 baseline.

**Beneficiary:** Users running `-END`/`-ENS` on JavaScript/TypeScript code bases, who pay the `ImpliedSemicolons` scan on every token.

**Scope:** The `ImpliedSemicolons` owner (`lizard_languages/javascript_statements.py`) and how ND/NS consume it: e.g. one scan shared by ND and NS, or statement ends found once by the reader. Gating `before()` to tracked tokens/line breaks was tried and gave no measurable gain (the cost is state work on the ~45% of tokens that are brackets or at line breaks). Deferred: changing ND/NS definitions.

**Key examples:**

- Sibling braceless `if`s without `;` still report ND/NS 1.
- ND/NS output on the SEED-066 frozen JS and mixed corpora is identical before and after.
- Paired API screens for `-END` and `-ENS` (40 pairs, 2 s batches) show a median below the current ~+22% against the pre-#578 master baseline, with a target bound recorded before work starts.

**Source:** SEED-066 approved #578 under a ≤25% median bound on 2026-11-20 and invited a cheaper follow-up on the PR.
