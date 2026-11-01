# PR #525 binding lifecycle correction

<a id="correct-pr525-binding-boundaries"></a>

### Correct, qualify and merge PR #525

**Identity:** SEED-023#correct-pr525-binding-boundaries
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/015-pr525-binding-boundaries/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"c6049a127a787d9d3122792a582d29627fc0d09c13330b85ad8dbdc11d7dc1c2","plan":"390db5744a48573e55376399cbbc48fd2b19249a06e21d2bff40bfcb97e58afc"}}
```

**Goal:** JS/TS users receive complete callback boundaries and complexity when
subscripts, object values and expression-bodied arrows share enclosing brackets;
maintainers receive PR #525 integrated with current master, refactored, qualified
below 3% slowdown and merged with a thank-you message.

**Beneficiary:** Lizard users affected by issue #524 and Lizard maintainers.

**Source:** https://github.com/terryyin/lizard/pull/525 and
https://github.com/terryyin/lizard/issues/524. The completed review is
SEED-022#review-pr525-issue524; its retained report is
`docs/reviews/pr525-issue524-20261101/README.md` and full evidence is recoverable
at `5f7d58fbad166bf05e1510162831bbc03cbabd96`.

**Scope:** Reconcile the contributor's binding lifecycle/replay correction with
current master without losing contributor history. Correct the remaining public
boundary and CCN failures; retain current arrow, formal-parameter, signature,
template, extension and CLI behavior. Independently refactor the aggregate change,
profile and remove unjustified runtime cost, run required local and hosted checks,
then merge the exact qualified contributor head and post one acknowledgement
describing material corrections. The user explicitly authorizes corrections,
publication to the contributor branch, merge and the thank-you message.

**Key examples:**

- The callback in `it('t', () => {\n e(r[0]);\n if(x) g(x);\n});` spans
  lines 1–4 and has CCN 2; a following function retains its own metrics.
- Chained calls such as `e(r[0]).t([{}])` do not close the enclosing callback.
- Array values in an object, including `return {w: [m[n - 1]]}`, preserve the
  enclosing function and its successor.
- All eleven authored cases retain their expected functions, complexity and
  spans for JS, TS, JSX and TSX, with shared MJS/CJS/Vue and NS consumers checked.
- Original crash, default initializer, typed/ternary arrow and template controls
  remain correct. Pre-existing label and deep-object limitations are not new
  promises, but introduced or exposed regressions must be dispositioned honestly.
- Each affected API/parameter consumer and broad CLI workload has a paired 95%
  upper slowdown bound strictly below 3% against frozen current master. Preserve
  original-parent/cumulative comparisons where applicable, immutable inputs and
  unfavorable attempts. No merge before semantic proof, independent refactoring,
  full pytest, make and applicable exact-head hosted CI pass.

**Constraints:** Preserve existing semantic owners and public metric meanings;
avoid parallel bracket counters or whole-stream buffering. Serialize Nix setup
and heavy measurements. No force-push. Stop dependent merge if the head moves.
