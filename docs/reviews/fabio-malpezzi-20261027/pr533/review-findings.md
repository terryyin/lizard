# PR533 review findings

The report is real and benefits users, but the complete proposal introduces
valid continuation errors and fails the affected performance gate. Defer to
[SEED-014#multiline-arrow-boundaries](../../../../.planning/seeds/SEED-014-multiline-arrow-boundaries.md#multiline-arrow-boundaries).
Correction and coherent cleanup are estimated at 30–45 minutes, above the
source review mission's fifteen-minute cutoff. No product correction was
attempted, and no PR action, comment, push or merge was performed.

## Frozen sources and review

- Authored head15423852c04aed302f684395fe9522251d88a2f2; original base01afe9c.
  Refreshed connector metadata still reports open/unmerged at that exact head.
  review-input.json retains the full body/metadata and proposed-patch.json the
  complete diff. Discussion comments, reviews and review threads are empty.
- The head has two commits: PR531's47aa357 and child1542385. The latter adds
  newline continuation tables, delayed metric transfer and per-token state
  snapshots. The complete proposal changes only two reader modules and ES6 tests.
- Historical authored-head run40418652237 succeeded on four jobs, recorded in
  hosted-checks.json. Current target7ac449f uses Ubuntu/Windows × Python3.8–3.14,
  fourteen jobs and pip install. No current candidate CI qualification is claimed.
- Fresh git fetch confirmed target7ac449f59b3f155fd159ce9aad6794a422045632.
  Relative to accepted execution incrementc56a2fe, upstream changes CI, support
  claims, records and a metadata test; analyzer files are byte-identical.
- ADR-0000 and ADR-0001 remain Accepted. Review follows the existing facade,
  function/binding/object/declaration owners and public metric meanings; no
  architectural exception or PR-specific production switch was introduced.

## Correctness observations

inputs.json supplies fixed valid source and explicit expected names, start/end
lines and CCN; the reported cases and semicolon controls also assert NLOC and
token count. reproduce.py compares every observation through analyze_file and
FileAnalyzer(get_extensions([])) and asserts that their results agree.

- reported.js/.ts/.jsx/.tsx: target arrows end at5/7, with first arrow CCN2;
  m has CCN2/NLOC6/tokens31; pick ends10/CCN3/NLOC2/tokens10. Raw has arrows4/6,
  CCN1/NLOC1/tokens5 each, m CCN3/NLOC8/tokens33 and pick11/CCN4/NLOC3/tokens15.
- async_field: target reports handler2–7 alone. Raw reports handler2–2,
  load3–5 and after7–7, all CCN1. Vue script-block controls preserve line offsets.
- semicolon/no_semicolon: raw has end1/NLOC1, counts4/3. Target's semicolon-free
  arrow extends to2/NLOC2/count4 by consuming the next statement's first token.
- multiplication, less_than, greater_than, exponentiation, call_continuation,
  subscript_continuation and tagged_template are valid two-line expressions.
  Target reports f1–2; raw ends f1–1. Continuation tables omit `*`, `<`, `>`,
  `**`, `(`, `[` and backtick as next-line starters. These are introduced errors.
  Less-than controls use JS/TS; other forms also cover JSX/TSX. Vue multiplication
  and call controls reproduce the same errors through the script consumer.
- postfix_new_statement, condition_next_statement and blank_comment distinguish
  a genuine statement boundary, including enclosing-function CCN ownership.
  operator_ends_line, parenthesized and block_body retain continuation/body controls.
- All64 inherited PR531 inputs remain in the corpus. Full raw retains invalid
  literal function-type returns (functions named return in TS/TSX/Vue) and the
  word/comparison object-operand problems documented by PR531. Raw533 happens to
  repair the outer end line for literal returns; the runtime arrow names remain
  wrong. There is no claim that PR531 or all its regressions were repaired.

The isolated independent-child snapshot removes only `_opens_an_object` and its
brace predicate. It retains arrow-expression state initialization/reset plus
the child behavior. All five original child tests pass, the reported fixes
remain, and string/template function-type returns recover. The same seven
continuation regressions persist. This disproves a necessary prerequisite on
PR531's invalid object heuristic, without accepting the diagnostic as a fix or
promising to solve SEED-013's object scope here. No formal story dependency is set.

The raw metric transfer also repeats ownership adjustments to CCN/NLOC/token
count after the pipeline has counted a token. A corrected approach must keep
the current owners coherent and avoid the measured unconditional bookkeeping
cost. Original ES6 tests grow to636 lines; correction must place just the added
tests in a cohesive module below250 lines, retaining unrelated test bytes.

## Literal reproduction commands and observations

External root R is `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533`.
The snapshots are R/target, R/raw and R/independent-child, constructed as in
README.md. These commands run from the prepared execution checkout; expand R
to that literal absolute path. Every invocation is through a real script.

```sh
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/raw
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/independent-child
```

Each has expected exit1 because the fixed corpus includes known failures.
The correspondingly named reproduction.log records every expected/actual row,
import source, FileAnalyzer parity and exact failing selection. This is a
diagnostic rejection, not green acceptance proof.

```sh
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target --suite authored
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/raw --suite authored
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/independent-child --suite authored
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/raw --suite focused
```

Authored assertions in authored-tests.py observe semicolon token differences,
bracket termination, next statements/methods, case/outer conditions and continued
expressions: target5fail, raw5pass, independent-child5pass. Setup supplies only
valid source, with no parser mocks. Focused selection in reproduce.py covers all
testJavaScript*, testTypeScript*, testES6, testTSX and testVue modules: raw278pass,
5skip. This existing-suite pass misses the introduced continuation regressions.

CLI observations use `python R/<snapshot>/lizard.py R/corpus/affected/<case>`
inside the same Nix environment, for target/raw/independent-child and cases
reported.js, async_field.js, multiplication.js and inherited_function_type_string.ts.
Twelve retained *-cli-*.log files expose the same public names/metrics and rows.
No product correction occurred, so an installed changed-effect consumer trace
is not applicable. The API, Vue and CLI observations trace the proposed effect;
future correction must run the installed consumer-trace and fresh proof gates.

## Fixed performance and profile

The originally measured paired.py, recoverable with the parent README's immutable recipe, has SHA256
3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5.
The external executed copy is byte-identical. manifest.json freezes150 related
inputs and100 original source files before analyzer runs or timing; all250
hashes are retained. API analyzes only the150 affected cases; CLI analyzes the
original lizard.py, lizard_core, lizard_languages and lizard_ext targets.

```sh
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/paired.py --baseline /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target --candidate /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/raw --workload api --pairs 20 --seconds 1.3 --out /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/533-api.json
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/paired.py --baseline /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target --candidate /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/raw --workload cli --pairs 20 --seconds 1.3 --out /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/533-cli.json
```

Both warm up, calibrate above1s and alternate20 pairs. Result JSON files retain
all durations, pilots, medians, standard deviations and paired log-ratio95%
t intervals. API median1.267039s→1.390600s, +9.7519%, interval[5.2695,11.8135]%:
fails. CLI median1.310213s→1.317860s, +0.5836%, interval[-0.4817,2.5769]%: passes.
The affected regression cannot be averaged away by the broader pass. No repeat
was used to seek a pass for already-invalid raw code.

Incremental target7ac and frozen baseline2e21 default analysis paths are
equivalent: only lizardns.py differs in analyzer sources, its initializer is
unchanged, and new NS methods are not called by default analysis. All150 API
summaries and complete broader CLI stdout/stderr/exit match exactly. The CLI
stdout SHA256 and comparison facts are in comparison-basis.json. Thus these
same measurements cover incremental and cumulative default cost. ENS is unmeasured.

```sh
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/profile-control.py /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target-profile.json
PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/profile-control.py /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/raw /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/raw-profile.json
```

One bounded5000-iteration unchanged-output block_body.js profile per snapshot
asserts identical six-field metrics. TypeScriptStates.__call__ has235000 calls
in each; self time0.05965s→0.08606s. The new line/function/complexity snapshots
run for every dispatched token, even on block bodies; this is plausible
overhead. Profile totals1.13786s→1.16163s and the control's different workload
do not isolate or fully explain the9.75% API result.

Actual host Territory.local, arm64,16CPUs,48GiB; pre-launch load3.62/2.37/4.09,
VM pages free10814, inactive796523, speculative6933 at16KiB/page. Other worktree
load was unknown. Coordinator avoided Nix; all owned heavy work ran sequentially,
one benchmark entry and one bounded profile pass. Reconsider on contention,
changed candidate or failed proof. The clear API rejection needs no noisy retry.

## Queue and remaining ownership

identity-observations.json retains installed identity-evidence reports for
executionc56a2fe (highest013) and clean current trunk7ac449f (highest011),
candidate014 unlisted, no history record/seed and gaps[]. The installed backlog
add operation queues SEED-014 after013, preserving existing maintenance order.
SEED-013's object outcome remains separate; common code does not create a wait.

The coordinator owns source acceptance, obligations, independent refactor,
final full pytest/make, delivery and any future hosted action. The queued story
has no Take or execution plan. Diagnostic candidate evidence does not qualify
corrected code, a future merge or ENS behavior. Harness setup mistakes and
corrected token expectations are retained in README.md and diagnostic logs.
