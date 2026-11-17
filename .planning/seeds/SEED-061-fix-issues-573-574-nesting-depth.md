---
id: SEED-061
status: active
planted: 2026-11-17
planted_during: lizard process issue
scope: ND for braceless if siblings and NS for else-if chains
---

# SEED-061: Fix issues #573 and #574 nesting over-count

<a id="fix-issues-573-574-nesting-depth"></a>

### Fix issues #573 and #574 — braceless `if` siblings and `else if` chains do not add nesting

**Identity:** SEED-061#fix-issues-573-574-nesting-depth
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"a3426949d4e33ac00edab0a86d0fa377c17c7d81234d372bcd1fb1b663965aab"}}
```

**Goal:** Users of the `-End` and `-Ens` extensions, and maintainers closing
[issue #573](https://github.com/terryyin/lizard/issues/573) and
[issue #574](https://github.com/terryyin/lizard/issues/574), get nesting
metrics that treat sibling braceless `if` statements and `else if` chains as
one level: ND does not grow with consecutive braceless `if`s, and NS does not
grow with each `else if`, matching the braced-equivalent results.

**Source:** Dashboard instruction to process open issues created in the past
48 hours. #573 and #574 were reported against lizard 1.26.1 by the same
reporter; no contributor PR exists. Bounded planless repair under
`lizard-process-issue` / `dough-bug-fixing`, then qualify the full repair with
`lizard-process-pr --local-change` before landing or wrap-up. Direction:
within "fix or improve existing functionality"; no external signature change.

**Beneficiary:** Users comparing ND/NS across code styles, and maintainers who
want ND and NS to agree on equivalent control flow.

**Scope:** ND (`lizardnd`) for consecutive braceless `if` statements and NS
(`lizardns`) for `else if` chains, in C/C++ and JavaScript as reported.
Deferred: other nesting changes, other metrics, and output formats.

**Key examples:**

- Given C++ or JS with four sibling braceless `if (x) f();` statements, ND is
  1 (as for the braced version), and NS stays 1.
- Given C++ `if {..} else if {..} else {..}`, NS is 1 and ND stays 1.
- Given JS with two `else if` branches in one chain, NS is 1 and ND stays 1.
- Genuinely nested `if` statements keep their current ND and NS.

**Starting revision:** `3feeac98` on `master`.

**Qualification:** After the shared refactor and before delivery or wrap-up,
qualify the entire repair with `lizard-process-pr --local-change`.
