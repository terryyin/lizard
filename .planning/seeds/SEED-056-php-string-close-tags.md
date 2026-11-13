---
id: SEED-056
status: dormant
planted: 2026-11-13
planted_during: issue 566 repair
trigger_when: selected issue processing
scope: small
---

# SEED-056: PHP strings preserve later functions

## Why This Matters

PHP users and PyDriller consumers need every function detected even when a
string contains a PHP closing tag. Issue #566 reports that only the first
method remains in Lizard 1.26.0 and 1.17.10 on Python 3.12.

## Story Decomposition

<a id="php-string-close-tags"></a>
### PHP string close tags

**Identity:** SEED-056#php-string-close-tags
```json dough-story-state
{"schemaVersion":1,"refinement":"not-refined","approach":"planless"}
```

**Goal:** Resolve the reported loss of PHP functions after `?>` in a string.

- **For / why:** PHP analyzer and PyDriller users need complete function names
  and source ranges.
- **Scope:** Diagnose and, if supported, repair the PHP reader in one bounded
  planless attempt; preserve actual PHP/HTML boundaries and existing metrics.
- **Expected:** A closing-tag sequence inside a quoted string remains string
  content, and later methods retain their names and source ranges.
- **Observed:** The reporter sees only `Example::first`; replacing `?>` with
  `? >` restores `second`, `third`, and `fourth`.
- **Evidence:** https://github.com/terryyin/lizard/issues/566; no discussion.
- **Uncertainty:** Confirm validity, exact cause, and affected literal forms.
- **Acceptance:** Demonstrate failing then passing analysis through the public
  analyzer, verify nearby literal and real closing-tag controls, and qualify
  the complete repair through `lizard-process-pr --local-change` before delivery.
- **Execution bounds:** `--no-replan`, ten-minute implementation attempt after
  investigation; required independent refactor and aggregate qualification.
- **Delivery:** Existing session worktree and branch; origin/master. Starting
  revision f9ae270e9892d63b204eebc8f90e6d695989b707.
- **Safe stopping point:** A verified repair preserving surrounding functions,
  or retained diagnosis and remaining work under this owner.
