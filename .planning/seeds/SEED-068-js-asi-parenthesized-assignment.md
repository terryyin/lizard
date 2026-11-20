---
id: SEED-068
status: active
planted: 2026-11-20
planted_during: SEED-066 wrap-up
scope: JavaScript reader — parenthesized assignment without a semicolon hides the next statement
---

# SEED-068: Parenthesized assignment without `;` hides the next `if`

<a id="js-asi-parenthesized-assignment"></a>

### Parenthesized assignment without `;` hides the next `if`

**Identity:** SEED-068#js-asi-parenthesized-assignment

**Goal:** In JavaScript/TypeScript without semicolons, an assignment of a parenthesized expression ends at its line break, so the following `if` is read and counted as its own statement.

**Beneficiary:** Users analyzing no-semicolon JavaScript/TypeScript (e.g. `js-yaml`) whose CCN, ND and NS miss or misplace the statement after such an assignment.

**Scope:** The JavaScript reader's handling of `name = (` as a possible arrow-function start when no `=>` follows and the line ends without `;`, and its effect on the following statement's metrics. Deferred: other ASI cases.

**Key examples:**

- In `js-yaml`'s `handleYamlDirective`, the `if` after `ready = (minor < 2)` (no `;`) is read as an `if`: same CCN/ND/NS as the semicolon version of that function.
- A minimal function with `ready = (minor < 2)` + line break + `if (ready) warn()` reports the same CCN, ND and NS as with `ready = (minor < 2);`.

**Source:** Reported in PR #578's description (one of 3 remaining ND mismatches across 11 no-semicolon npm packages); deferred from SEED-066 on 2026-11-20.
