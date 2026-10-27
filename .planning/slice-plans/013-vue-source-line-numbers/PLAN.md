# Preserve original Vue source line numbers

**Identity:** SEED-017#vue-source-line-numbers
**Source:** [Refined story](../../seeds/SEED-017-vue-source-line-numbers.md#vue-source-line-numbers)

## Outcome and boundaries

Vue users receive original one-based function ranges and warning locations when
template or other ignored content precedes script code. Public analysis and the
real CLI agree. Retain existing script function detection and metrics, including
JavaScript and TypeScript script content, script-first components, and supported
multiple script blocks. Ignored content contributes source line breaks without
becoming analyzed code.

No arrow parsing/parameter changes, expanded Vue grammar, source maps, new output
options, or sibling arrow-story outcomes are promised. No supplier dependency is
required. This preparation authorizes no implementation; the slice is planned.

## Existing owners and direction

PFE: change the existing Vue filtering responsibility in
`lizard_languages/vue.py` (`VueReader.code_tokens` and `_skip_to_script`).
`TypeScriptReader.preprocess` consumes that filtered stream before the shared
`comment_counter` and `line_counter` in `lizard_core/file_analyzer.py` calculate
locations. The filter currently discards newlines outside script blocks.

The existing shared rule already distinguishes line breaks from code tokens:
`line_counter` advances on individual `\n` tokens without charging NLOC or token
counts. C-like macro preprocessing and comment processing also preserve discarded
content's line breaks. Reuse this rule by preserving the newline count of every
filtered token, including tokens containing embedded newlines and script tags.
Do not maintain a second line offset or repair locations in individual formatters.
Keep ordinary script tokens flowing through the existing TypeScript pipeline.

Consumers use `FunctionInfo.start_line`/`end_line`: `location` feeds default text,
`OutputScheme` feeds warning locations, and CSV/XML/checkstyle output reads the
same fields. The website renders those fields without its own Vue extraction.
Correcting the reader therefore corrects the source values for these consumers;
no formatter-specific or frontend change is needed. At implementation, run the
installed consumer trace against the actual diff and account for each reached
consumer under the execution workflow.

Use [ADR-0001, Ubiquitous language](../../../docs/adrs/0001-ubiquitous-language-accepted.md)
for language reader, function, NLOC and CCN terminology. The Accepted ADR index
and records agree; no conflicting decision or supersession was found. This
local correction stays within established ownership and needs no new architecture
decision or North Star topic.

## Observed premises

Observed on 2026-10-27 in
`/Users/terryyin/git/lizard/.worktrees/preserve-original-vue-line-numbers-when-template`,
product revision `b37648adf90b64a71579a55965f030f85b14ea06`.

- `nix develop -c python /tmp/lizard-vue-line-premises.py` analyzed actual Vue
  source through `analyze_file.analyze_source_code`, then wrote a temporary Vue
  file and invoked `python lizard.py <file>` and `python lizard.py -w -C1 <file>`.
  A six-line template/blank prefix followed by `<script>` and named functions
  reports 2–5 and 7–9 instead of source ranges 8–11 and 13–15. Inserting four
  blank/template lines leaves the wrong reports unchanged instead of shifting
  to 12–15 and 17–19. CLI output repeats the wrong ranges; warning exit 1 points
  to line 2 instead of 8. Script-first control ranges 2–5 and 7–9 are correct.
- `nix develop -c python /tmp/lizard-vue-line-experiment.py` first ran the
  original-location assertions against the unchanged reader and observed their
  failure. It temporarily replaced the Vue filter with a script-state filter
  emitting one `\n` per discarded source newline, reran the public API and CLI
  observations, and restored `vue.py` byte for byte in `finally` before record
  writes. All seven API cases passed: template-first, four-line shift,
  script-first, TypeScript, a multiline ignored attribute token, multiple
  scripts separated by style content, and no script. Later-script function
  `third` moved from the incorrect 11–13 to source lines 21–23. Default CLI
  locations became 8–11 and 13–15; warning exit 1 pointed to line 8. Default
  exit 0 and empty stderr were preserved.
- The experiment compared every function's name, NLOC, CCN, token count and
  parameter count, plus file NLOC, against the unchanged reader: all matched.
  It also ran `python -m pytest -q test/test_languages/testVue.py
  test/test_languages/testTypeScript.py test/test_languages/testJavaScript.py
  test/test_analyzer.py test/testOutput.py test/testApplication.py` against the
  scratch candidate: **102 passed**. These are feasibility and regression-route
  observations, not accepted implementation or permanent regression tests.

The temporary observation scripts above are outside the repository. Durable
regressions belong in the slice below. No scratch product or test edits remain.

## Proof ownership and gates

| Promise | Owner and observable proof |
| --- | --- |
| Original start/end lines and proportional prefix shifts | Slice 1: named-function regressions through public analysis with explicit original source ranges, red before the filter change and green after |
| Default CLI locations and warning navigation | Slice 1: real temporary-file subprocess checks of default `name@start-end@path` output and `-w -C1` `path:line: warning:` output, including exit status |
| Existing JS/TS scripts, embedded newlines and gaps between script blocks | Slice 1: public analysis checks of the refined examples, including an ignored multiline token and a later script |
| Preserved metrics and filtering boundaries | Slice 1: compare names, NLOC, CCN, token counts and parameter counts for equivalent script content; script-first and no-script controls; ignored content creates no functions/code metrics |
| Shared pipeline and output consumers | Slice 1: the focused suites below, actual-diff consumer trace and full-suite completion gate |

Follow [AGENTS.md](../../../AGENTS.md): tooling through Nix, public entry points,
and mocks only for filesystem boundaries. Apply `lizard-language-support` for
reader/test changes. Focused feedback:

```sh
nix develop -c python -m pytest test/test_languages/testVue.py test/test_languages/testTypeScript.py test/test_languages/testJavaScript.py test/test_analyzer.py test/testOutput.py test/testApplication.py
```

Before completion run `nix develop -c python -m pytest` as required by AGENTS.md;
run `nix develop -c make` for the repository's full test and style checks.
Execution owns independent post-change refactoring, review, delivery and CI
under its normal workflow. No project-specific numeric slice limit was supplied.

## Ordered slices

### 1. Keep original locations while filtering Vue content
Type: Behavior
Status: done
Proof: red→green public source-range and real CLI regressions, unchanged metrics
and script-first/no-script controls, focused suites and required completion gates.

Behavior: given a supported Vue component whose ignored content precedes or
separates script code, analyzing it returns original function ranges and emits
those locations in default output and warnings, while retaining script metrics
and excluding ignored content from analysis.

Add the discriminating regressions at the public boundary in
`test/test_languages/testVue.py` and a real CLI integration test outside the
mocked `TestApplication` class in `test/testApplication.py`, following the
existing temporary-directory/subprocess pattern. Use ordinary named functions
to isolate this defect. Cover the source examples and controls in the same
proof loop. Keep the filtering rule in the Vue reader, preserving individual
newline tokens for discarded content; remove skip machinery made redundant by
that coherent rule. Verify and refactor the cohesive change before delivery.

Boundedness: one existing filtering owner, one location-preservation rule and
one outside-in regression loop. Tests and CLI observation verify the same
outcome, so splitting them would leave that outcome partially unproved. No
preparatory Structure slice or interim behavior is needed. A safe stopping
point is the fully verified correction, independent of the arrow stories.

#### Accepted proof (slice 1)

```
proof:
  command: nix develop -c python -m pytest test/test_languages/testVue.py test/test_languages/testVueSourceLineNumbers.py test/test_languages/testTypeScript.py test/test_languages/testJavaScript.py test/test_analyzer.py test/testOutput.py test/testApplication.py
  covers: original Vue ranges, prefix shift, metrics, JS/TS/script-first/no-script/multiline/multi-script, default CLI and -w -C1 warning line, shared analyzer/output consumers
  boundary: VueReader.code_tokens newline preservation through TypeScript preprocess and FunctionInfo locations
  observations:
    - test/test_languages/testVueSourceLineNumbers.py::TestVueSourceLineNumbers: original ranges 8–11/13–15, shift 12–15/17–19, TS, multiline, multiple scripts third 21–23, metrics, script-first, no-script
    - test/testApplication.py::TestVueSourceLineNumbersCli: first@8-11 / second@13-15; warning path:8:; exit 1
  setup: none (fixtures are inline Vue sources)
  result: pass (111)
```

Product paths: `lizard_languages/vue.py` (`code_tokens` emits one `\n` per discarded-token newline; `_skip_to_script` removed). Tests: `testVueSourceLineNumbers.py` (extracted), `testApplication.py` CLI class. Post-change refactor collapsed duplicate newline yields and split the oversized Vue test module.

#### Learnings

- `-C1` warns only when CCN > 1; named-function fixtures need a decision for the CLI warning journey.
- Discarded tokens must emit one `\n` per embedded newline (including tags), matching shared comment/macro line preservation — no second offset.
- Line-number fixtures belong in `testVueSourceLineNumbers.py` once they push `testVue.py` over the 250-line limit.

## Plan review

Slice-plan refinement was not needed: one cohesive slice covers one rule and
its API/CLI consumers; the scratch observation settled the approach and proof
route. Cumulative design preserves the shared line counter and one Vue filter
rather than adding output-specific offsets. No remaining slice-specific concern
or human decision was identified.
