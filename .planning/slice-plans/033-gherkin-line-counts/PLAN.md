# Gherkin written-block line counts

**Identity:** SEED-055#gherkin-support
**Source:** [Report line counts for Gherkin feature files](../../seeds/SEED-055-gherkin-support.md#gherkin-support)
**Source review:** [source-review.md](source-review.md)

## Goal and scope

Let teams measure the written size of Gherkin scenarios and shared setup using
Lizard's existing API, reports, and NLOC warnings. The source seed owns the
contract and examples: one function per written Background, Scenario/Example,
or Scenario Outline/Scenario Template; each has CCN 1. An outline owns its
Examples/Scenarios tables once, and Background is never copied into consumers.

Recognize English `.feature` files through ordinary discovery and `-l gherkin`.
Count each block's declaration and nonblank, non-comment contents; preserve
titles and source locations. Containers, descriptions, tags and opaque step
arguments must not leak lines or fabricate declarations. Include rule-level
blocks, unnamed Backgrounds, duplicate titles, and the final block at EOF.

Localization, execution/step resolution, AST validation, malformed-input
diagnostics, row expansion, and a new complexity/token/parameter metric are
deferred. These are delivery limits, not instructions to reject naturally
supported input. Ordinary Lizard read-error handling stays in force.

## Existing solution and decisions

PFE examined the whole applicable path: `lizard_languages/__init__.py`,
`CodeReader`, the existing Python/Fortran/TTCN reader mechanisms,
`lizard_core/file_discovery.py`, `file_analyzer.py`, `file_info_builder.py`,
`function_info.py`, `extensions.py`, API/language/CLI tests, packaging, and the
website's Cucumber dependency. No existing Gherkin analyzer was found.

Reuse registered reader discovery, function records, shared NLOC/comment/token
counters, warning thresholds, and reporters. Add Gherkin lexical/block ownership
in one language reader rather than teaching core counters or CSV about Gherkin.
The website's Node Cucumber test runner owns website proof, not Python source
analysis; reuse of that runtime would add an unrelated service/process boundary.
A full AST package is a larger alternative whose dialect/validation benefits
are outside this increment. No new dependency is needed by the observed approach.

Use one coherent rule: declarations start source-owned blocks, containers and
following declarations end them, and literal arguments remain opaque. Tags must
be associated before deciding which block owns their lines: tags before a new
scenario stay outside its predecessor, while tags on Examples remain inside an
outline. Do not create a separate parser for each example layout.

The prototype used whole-line tokens and a preprocessing seam before shared
counters. It demonstrated a feasible seam, not a requirement to retain its
implementation. `FunctionInfo` starts with NLOC/token count 1; starting a
function before the header reaches counters needs deliberate initialization to
avoid counting that header twice. Finalizing the previous function must also
precede counting the next declaration or container.

The ADR index and both current Accepted records agree. Follow
[ADR-0001, Ubiquitous language for Lizard domain concepts](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
for Function, Language reader, NLOC and CCN, and
[ADR-0000, Use ADRs](../../../docs/adrs/0000-use-adrs-accepted.md) for human
decision ownership. There is no conflict, exception, or warranted cross-cutting
direction; this ordinary reader addition needs no North Star topic or ADR draft.

Expected existing edits are the reader registry (75 lines) and `docs/cli.rst`
(128 lines), measured at the preparation base. New reader and proof files have
cohesive responsibilities and stay within the 250-line refactor check. No edit
to the already 271-line README is necessary: the maintained CLI reference is
already linked from it. If a different existing oversized file becomes an
expected edit, settle its cohesive reorganization before implementing that edit.

## Outside-in proof ownership

All promises belong to Slice 1. Tests use real parsing through
`analyze_file.analyze_source_code`, `analyze_file`, and CLI subprocesses;
mock only filesystem boundaries if needed, per `AGENTS.md`.

| Source promise | Observable proof |
| --- | --- |
| Written Background/Scenario/Outline, not executed cases | Seed example 1 yields three functions with NLOC 2/3/7, CCN 1, declaration lines 2/5/9 and end lines 3/7/15 |
| Shared setup and Examples ownership | Multiple scenarios do not acquire Background lines; multiple tagged Examples sections and rows enlarge only their single outline |
| Nonblank, non-comment source size | Black-box fixture with descriptions, comments, blank lines, step `#` and C-like punctuation, and data tables yields exact function and file NLOC |
| Literal content cannot create functions/comments | Parameterized triple-quote/backtick fixtures retain header-looking and `#` lines as data, skip blank content, and preserve CCN 1; table-cell text is also opaque |
| Accurate block boundaries and identity | Rule/Feature headers and descriptions do not enlarge the preceding function; tags preceding a Scenario do not leak; aliases, unnamed Background, duplicate titles, empty input and no final newline retain correct results/locations |
| Existing discovery and reporting reach users | A temporary directory containing `.feature` and another language produces Gherkin rows in ordinary CSV; `-l gherkin` selects them; direct-file API agrees |
| Existing NLOC warning integration | A controlled feature block above `-T nloc=<limit>` produces the existing warning and expected CLI status; an otherwise equal block at the limit does not |

Keep the test fixtures focused on distinguishing ownership rules, rather than
repeating every metric assertion in every case. The CLI test proves discovery,
reporting and thresholds; parser variations belong at the public API boundary.
The final regression tests must fail when the new reader is absent/disabled.

## Decisive premises and observations

Observations were made on 2026-11-11 in the named preparation workspace on
`9db4c39d51e17d7e2732f159175c277e7cf2e92a`, through `nix develop -c python`.
The scratch reader, registry edit and temporary pytest module were removed
before any preparation record write. The scratch was a feasibility experiment,
not delivered implementation or accepted final proof.

| Premise / consuming operation | Form, profile and observed result | Unverified remainder |
| --- | --- | --- |
| Existing analyzer lacks the promised output | `analyze_file.analyze_source_code('basket.feature', <seed example 1>)`: reader None, file NLOC 13, zero functions; `python lizard.py <temporary directory> --csv` and explicit file each emit no rows | Settled baseline gap |
| A registered reader can use current counters and expose block metrics | Disposable `gherkin.py` plus actual registry wiring; `python -m pytest -q test/test_languages/testGherkinPreparationProbe.py`: seed example asserted 2/3/7 NLOC, CCN 1 and ranges 2–3 / 5–7 / 9–15; passed after correcting seeded header counts | Final reader and permanent tests are Slice 1 work |
| The same seam can preserve literal/container/EOF ownership | Same pytest invocation asserted a quoted Scenario/`#` payload with a blank content line, followed by Rule, Background and Scenario without a trailing newline: NLOC 7/2/2, ranges 2–9 / 11–12 / 13–14; passed | Tagged boundaries and other delimiter form remain explicit final proof, not completed behavior |
| Normal discovery/filter/report path consumes a new reader | With the disposable reader, `python lizard.py <temporary directory> --csv`, then the same command with `-l gherkin`, each exited 0 and emitted exactly three rows with NLOC 2/3/7 and CCN 1 | No reporter/core change required; warning behavior is final acceptance |
| The representative proof detects the missing capability | Same scratch pytest invocation before registry/reader addition exited 1 (zero functions); with the reader it exited 0, 1 passed in 0.03 s | Permanent regression still owned by Slice 1 |

These are the approach's current-state premises. Future behavior assertions
such as tagged Examples ownership are implementation acceptance, not assumed
existing capabilities. No paid, credentialed, host-held, or external premise
requires an early probe or pre-Take human decision.

## Ordered slices

### 1. Report written Gherkin blocks through ordinary analysis

Type: Behavior
Status: done

**Pre-condition:** A team has English `.feature` source containing scenarios,
shared setup, outline data, comments, tags and multiline step arguments.
**Trigger:** The team analyzes a file or directory, optionally selecting
`-l gherkin` or an existing NLOC warning threshold.
**Result:** It receives one function per written block, accurately owned NLOC
and locations, CCN 1, and the normal report/warning behavior in the source seed.

Add the reader and registry entry together with public API tests under
`test/test_languages/testGherkin.py` and focused CLI proof in a new
`test/testGherkinCLI.py`. Preserve shared counter semantics for all other
languages. Respect both literal delimiters and table content before recognizing
headers or comments. Ensure block finalization and upcoming metadata ownership
occur at the correct point relative to shared line counting.

Extend `docs/cli.rst` with `.feature` usage, `-l gherkin`, block NLOC examples,
the written-source meaning of Background/Outline and CCN 1, and the deferred
English-only/validation boundary. This is the maintained home for lasting
Gherkin semantics when the seed/plan are removed at wrap-up.

**Proof:** Implement the mapped observations above; exercise the real CLI with
temporary source files and parse emitted rows/status, without mocking parsers.
Start with failing observable examples, implement one common ownership model,
and apply post-change refactoring before final verification and commit.

**Local verification:**

```
nix develop -c python -m pytest test/test_languages/testGherkin.py test/testGherkinCLI.py test/testLanguages.py
nix develop -c make
```

The full pytest suite is required before completion by `AGENTS.md`; `make`
runs it through coverage and adds repository style checks. Trace changed
consumers through execution's installed consumer-trace procedure, then run any
additional directly affected tests. Use the execution host's current workload
evidence before expensive proof. Hosted checks remain with execution's existing
CI ownership; preparation starts no observer.

**Sizing and safe stopping point:** One reader capability with one outside-in
ownership/reporting proof loop; no separate metric, formatter or framework.
Literal handling and tag ownership protect the same promised block counts, so
they stay with the reporting slice. No intermediate partial reader is delivered.
The project supplies no numeric slice target here; boundedness follows this
single outcome, modest touched files, and the settled pipeline seam. If actual
evidence invalidates it, replan within the story or surface changed scope.

## Current decisions

- Use the English source boundary already represented by the seed's keywords;
  aliases follow their documented Gherkin equivalence. Localization is deferred
  and no unsupported-language rejection is introduced.
- Reuse the existing result/counter/report model; keep new domain recognition
  in the reader. No runtime step dependency or syntax-validator contract.
- Execution identity: story-branch `cursor/report-line-counts-for-gherkin-feature-files`,
  published claim base `2686957cf3c66ebc8280717b52848b0623ab43af`, agent Yuma-chan.

## Learnings

- Opening a function before its header passes through counters initially
  double-counted the header in the scratch probe. Correct initialization
  restored the expected counts without changing shared counters.
- `.feature` files are absent from directory discovery until reader registration;
  API-only parsing proof would miss this user path, so CLI proof is mandatory.
- Pending tags must be flushed through counters on `*global*` before
  `restart_new_function`; opening the next block first attributed scenario tags
  to the successor.

## Accepted proof (slice 1)

```
proof:
  command: nix develop -c python -m pytest test/test_languages/testGherkin.py test/testGherkinCLI.py test/testLanguages.py
  covers: written-block NLOC/CCN/locations, opacity, aliases, CLI discovery/filter/warnings, registry
  boundary: analyze_file.analyze_source_code / analyze_file / lizard.py subprocess
  observations:
    - test/test_languages/testGherkin.py TestGherkinSeedExample1: NLOC 2/3/7 CCN 1 ranges 2–3/5–7/9–15
    - test/test_languages/testGherkin.py ownership cases: Background isolation, tagged Examples, opacity, Rule/EOF/aliases
    - test/testGherkinCLI.py directory CSV, -l gherkin, -T nloc warning threshold
    - test/testLanguages.py .feature → GherkinReader
  setup: inline SEED_EXAMPLE_1 and temporary directory fixtures in tests
  result: pass (23 passed; coordinator and post-refactor re-verified)
```

Full suite evidence from implementation: `nix develop -c make` → 3144 passed, 7 skipped; pylint 9.20/10.

## Plan review

Slice-plan refinement was not needed: one cohesive Behavior slice owns the
common block model, its syntax-boundary cases, reporting, proof and documentation.
No remaining slice-boundary, cumulative-design, proof-mapping, or sizing concern
was identified. No source decision or dependency blocks preparation completion.

## Execution complete

Product advice: no change — written-block Gherkin NLOC through ordinary
analysis matches the seed; deferred localization, validation, expansion, and
metrics remain deferred. Process review found no supported new DearDough
finding.
