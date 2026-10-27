# Arrow parameter execution context through slice 2

Detailed context and preparation review moved from the active plan during the
independent slice 2 refactor. Canonical slice statuses and story obligations remain
in [PLAN.md](../../../../.planning/slice-plans/012-arrow-parameter-bindings/PLAN.md).
Historical observations retain their original dates, revisions and limits.

## Execution state and accepted proof

- Identity: `SEED-016#arrow-parameter-bindings`; publisher
  `dashboard-territory.local-lizard`; assigned agent `Sola-chan`.
- Execution and originating checkout:
  `/Users/terryyin/git/lizard/.worktrees/report-consistent-javascript-and-typescript-arro`;
  branch `codex/report-consistent-javascript-and-typescript-arro`, Story Branch
  Mode. This is the established owned linked worktree; no integration checkout
  is selected for mutation. Authorized remote/trunk: `origin` / `master`.
- Starting revision: `65c1a10c5a2509188957af6abbc16353442a2dfc`; accepted claim,
  previously published branch base and initial candidate:
  `7f479101728b5fec8becaf7c784b752ed736719e`.
- Checkout setup: checked-in `shell.nix` creates the local venv and installs
  `dev_requirements.txt`; `nix develop -c python -m pytest
  test/test_languages/testJavaScriptParameterNames.py` passed 34 tests here.
  Nix commands remain serial. No selective formatter or commit hook is installed;
  `make` owns the required style checks. Workspace author is `Sola-chan`.
- CI: GitHub Actions `python-app.yml`, verified active ID `38306292`, display
  name `Python application`; push trigger covers the execution branch.
  Codex observer coordinator `sola-seed016`, directory
  `/tmp/dough-ci-501/watch-TEDK9c`, PID `99850`, PTY session `74466`, yielded
  continuation cell `32`. Request root/repository/branch and owner hash were
  checked against this checkout and coordinator after recovering the header.
  It covers ordinary branch increments; the initial trunk claim is unobserved.
- No numeric slice hard limit is configured. Replanning retains the plan's
  existing authority within this story; product promises remain unchanged.
- Source-only acceptance review: fresh non-inheriting `source_commitments`
  agent received the source Goal, Scope, examples, representation, completion
  and explicit execution amendment, no plan/product/return or project-file
  access. Its launch and 72 returned commitments are retained in the execution
  conversation. Compared with the actual source: counts/names, all four-arrow,
  alias, empty-pattern and typed examples, reader/consumer coverage, controls,
  representation/ownership, performance, checks and exclusions remain intact.
  They are future obligations of slices 2–5, not results of this route probe.
- Slice 1 expected route: the intended open contribution can receive a qualified
  correction without losing contributor history. Actual observation: unchanged
  open head, maintainer editing enabled, matching fork ref and authenticated
  receive-pack dry-run exit 0. Commands, observed refs/checks/discussion and
  preview limitations: [contributor-route.md](contributor-route.md).
  This accepts feasibility only; changed-candidate push acceptance and final
  head/CI/merge/acknowledgement remain slice 5's obligations. No product tests
  are claimed for this evidence-only slice.
- Slice 1 accepted publication: `33610866db00d28ba9e2dafe5a3fd1942347fb2f`
  on `origin/codex/report-consistent-javascript-and-typescript-arro`, suffix
  base `7f479101728b5fec8becaf7c784b752ed736719e`; managed observation reused
  the coordinator's stream. Independent route refactor returned no candidates.
- Slice 2 source-only review: fresh non-inheriting `slice2_source_review` read
  only the original seed section and explicit execution amendment. Its complete
  commitment inventory and launch are retained in the conversation. Comparing it
  to the source, product diff, test setups/assertions and consumer paths accepts
  the four-arrow example, alias examples, compound/nested/quoted/computed patterns,
  sigils, bare/default/rest and wrapper formals through shared extraction.
  Exact API values across JS/TS/JSX/TSX/Vue/cjs/mjs, real CLI thresholds,
  CSV/checkstyle/HTML counts, duplicate alias keys and Flask JSON are observed by
  `testJavaScriptArrowParameters.py` as indexed in
  [slice2-proof.md](slice2-proof.md).
  All 34 accepted controls pass. The source's empty-slot and typed-return journeys
  have concrete remaining mismatches owned below; performance/final delivery
  remain required. No browser rendering proof is claimed: the unchanged frontend
  directly renders the verified numeric JSON field.
- Slice 2 proof: initial 18 regressions red before product edits; expanded reader
  and consumer selection passed 572 tests, 5 skipped. Frozen 65-case reproducer
  plus actual CLI completed with 60 passing rows and exactly the five retained
  failures in `slice2-candidate.jsonl`. The baseline API run's missing checked-in
  corpus setup failure is retained; corrected cache inputs are byte-identical.
  Literal commands, inspected observations and all trace dispositions are in the
  proof record. The facade and unrelated large tests are unchanged.


## Existing owners and architectural direction

PFE: change the existing anonymous-arrow path in
`lizard_languages/javascript_bindings.py` (`JavaScriptBindingsMixin` pending
binding summaries and `_push_arrow_function`) and
`lizard_languages/typescript_states.py` / `javascript_functions.py`
(`_dec` parameter extraction, `_arrow_function`, binding-candidate
parentheses), together with `lizard_core/function_info.py` (`FunctionInfo`
`parameters` / `parameter_count` / `full_parameters`). TypeScriptReader
composes those owners; JavaScript, JSX/TSX and Vue script reuse them;
`.cjs`/`.mjs` alias the same reader. Do not add a parallel pending-name
collector; keep the public facade.

Rejected shapes from the review: raw/adapted #536 keep a second
`_read_name` collector beside `_dec`, so anonymous aliases expose property
keys instead of the named convention; empty patterns under-count; typed
callback annotations emit phantoms; and paired affected API / names-counts
gates fail. Prefer one coherent rule: when a binding candidate or bare
identifier becomes an arrow, formal slots and binding names enter
`FunctionInfo` through the same representation named signatures already use.

Settled public representation (from the refined story): `parameter_count` is
formal slot count; `parameters` lists established binding names only; empty
patterns add a slot and no name, so count may exceed `len(parameters)`;
trailing-comma empty tokens stay excluded.

Follow [ADR-0001, Ubiquitous language](../../../adrs/0001-ubiquitous-language-accepted.md)
for parameter count / language reader / function terminology, and
[ADR-0000, Use ADRs](../../../adrs/0000-use-adrs-accepted.md)
for human-owned architectural decisions. Both are Accepted and unsuperseded.
Established ownership supports this local correction; no North Star topic or
ADR is warranted. No architectural exception is proposed.

SEED-014 and SEED-015 are Taken and change related expression-body state. At
Take, refresh trunk and rerun the premise observations below on the execution
checkout; reconcile with whichever landed rather than reintroducing their
deferred outcomes. No formal blocking dependency is asserted.


## Premises observed during preparation

Checkout: `/Users/terryyin/git/lizard/.worktrees/report-consistent-javascript-and-typescript-arro`,
revision `0ef95abbe278bfd22bbe4de48b9fa4b71f7b2181`, observed 2026-10-27.

- Through `analyze_file.analyze_source_code`, the four reported anonymous
  arrows expose counts `0,0,0,0`. Named `({key: renamed}, tail) =>` reports
  `['renamed','tail']` count 2 while the anonymous twin reports `[]` count 0.
  Named `({}, [], tail) =>` reports count 1 / `['tail']` with
  `full_parameters` `[',', ' tail']`; the anonymous empty form reports
  count 0. Typed callback input on this revision reports one anonymous
  count 0 (review target `d7be8b9` had a phantom zero-parameter sibling as
  well); correction must still yield one function count 4 with names
  `['number','$','cb','tail']` and no phantom. Member assignment remains
  count 0; wrapper-call anonymous also count 0 on target.
- Inspected `JavaScriptBindingsMixin._push_arrow_function` (transfers only
  CCN/defaults from pending summaries, not names) and
  `JavaScriptFunctionsMixin._dec` (named signatures call
  `context.parameter`). Confirms the owners named above. Adapted #536's
  parallel `names` / `_read_name` collector is the rejected shape.
- `FunctionInfo.parameter_count` is `len(self.parameters)`, and `parameters`
  drops tokens that yield no identifier match — so empty formal slots cannot
  be represented without extending that shared model.
- `gh pr view 536 --repo terryyin/lizard` returned `OPEN`, head
  `92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f` on FabioMalpezzi's fork, base
  `master`, `maintainerCanModify: true`. Readable metadata is not accepted
  publication; slice 1's early probe owns permission and route.
- Frozen evidence tree exists under
  `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536` and baseline under
  `/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline`. Cache
  `paired.py` SHA256 is
  `3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5`; the
  review record's central `paired.py` hash is
  `ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`. Before
  timing, confirm the harness bytes used match the review's intended
  measurement script (restore or pin if the cache copy has drifted). This
  settles replay reachability, not any candidate's timing.


## Plan review

Slice-plan refinement was not needed: slices separate a permitted early
external probe, unified anonymous capture through existing owners, the
slot/phantom extension with its own regression loop, the independent
performance loop and hosted delivery. The cumulative design keeps one
FunctionInfo parameter representation and rejects the parallel collector.
No remaining slice-specific concern was found; contributor publication is
bounded by the early probe. No human decision is currently pending.
