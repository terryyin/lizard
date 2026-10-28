# PR #532 review and bounded correction

Disposition: merged9bc44563aad42b780ff0264aa5a3a972a583b423, corrected head
2a8af8acea20c53a0c57c75ff5d179d2db8a2eff, with all14 required CI jobs successful.
Thank-you: https://github.com/terryyin/lizard/pull/532#issuecomment-6436143243.

Source: https://github.com/terryyin/lizard/pull/532. Original and refreshed head
`d0713bf5b4dcf4bf24453156a3756688627abbc0`, base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`; initially open, no discussion,
review submissions or inline threads. Its historical run 40418648778 passed
four jobs, which does not qualify the current fourteen-job matrix.

Review target: `33807306953535ebda5c0fa42797b1e5e7ef73d1`.
Fresh trunk advanced from 7ac449f to
`d8e8e9af75b6d05da005e981af5f8867cced5d55` through the website revival.
The complete lizard.py/lizard_core/lizard_languages/lizard_ext diff between
those trunk revisions is empty. The new website parameter consumer is
separately traced; timed analyzer inputs and code remain immutable.

## Public behavior and value

The current target reports render(text,_key,$el)=1 and
round(number:number,precision:number)=1; valid source requires 3 and 2.
These counts affect CLI output, warning thresholds, formatters and duplicate
parameter-list keys. The raw PR fixes both and its authored examples, but is
incomplete and introduces three valid-source regressions:

| Source shape | Target | Raw PR | Corrected | Required |
| --- | ---: | ---: | ---: | ---: |
| render(text,_key,$el) | 1 | 3 | 3 | 3 |
| round(number:number,precision:number) | 1 | 2 | 2 | 2 |
| names($,$$,a$,a$b,_,_x,$x,x1,café) | 4 | 6 | 9 | 9 |
| unpack({'key':renamed},tail) | 2 | 1 | 2 | 2 |
| unpack({1:renamed},tail) | 2 | 1 | 2 | 2 |
| unpack({[key]:renamed},tail) | 2 | 1 | 2 | 2 |

The raw colon flag treats destructuring's property colon as a type annotation.
The disclosed lone-$ loss is wider: the tokenizer splits $$ and trailing/infix
dollar identifiers, and FunctionInfo's word-only matcher drops trailing dollars
or strips sigils from normalized names. Parser-local state is insufficient.

## Small coherent correction

Before product edits, the correction plus cleanup was estimated at 12–15
minutes, excluding review and proof. It remains that bounded correction:

- JavaScriptFunctions retains the contributor's identifier and annotation idea,
  removing the type-keyword name blacklist.
- JavaScriptBindings exposes its existing root-depth knowledge, so only a
  parameter-list root colon starts an annotation.
- One JS_IDENTIFIER token pattern serves TypeScript and JSX/TSX, preserving
  complete dollar-containing names; Vue uses the TypeScript tokenizer.
- FunctionInfo's existing matcher accepts dollar characters. There is still one
  public parameter list and count; no reader-specific core mode is added.
- One small test module covers observable counts/names and their consumers;
  the original larger author test modules remain byte-identical here.

The established outer-only ungrouped curried-arrow convention is unchanged.
No unrelated arrow, newline or this-declaration correction was applied.
ADR-0000 and ADR-0001 remain Accepted: core ownership/public facade and metric
meanings are retained. Their records are docs/adrs/0000-use-adrs-accepted.md
and docs/adrs/0001-ubiquitous-language-accepted.md. No exception is needed.

## Proof and actual observations

The red snapshot test used the new public-analyzer module before production
edits: target 10 failed/23 passed; raw on target 20 failed/13 passed.
Raw final focused proof, including the two authored tests, reports 20 failed,
1447 passed and 5 skipped: the added discriminating regressions explain failure.
No assertion was weakened. Corrected isolated focused proof passed 1465/5.

Literal expanded command, run on the frozen correction:

```sh
PIP_QUIET=1 nix develop -c python -m pytest -q test/test_languages test/test_extensions test/test_analyzer.py test/testOutput.py test/testApplication.py test/test_options.py
```

All reader and extension suites are selected, not a filtered sample. The
additional real consumers were observed with:

```sh
PIP_QUIET=1 nix develop -c python -m pytest -q test/test_languages/testJavaScriptParameterNames.py test/testOutputCSV.py test/testOutputHTML.py test/testCommentOptions.py
```

That command passed 51 tests, including all 34 new cases. In
test/test_languages/testJavaScriptParameterNames.py:

- test_identifier_boundaries asserts actual parameter names and count 9 across
  JS, TS, JSX, TSX and Vue through analyze_file.
- test_keyword_names_and_annotations asserts named keyword parameters and
  excludes nested generic, callback and inline-object annotation tokens.
- test_binding_colons_are_not_annotations asserts count 2 for five binding
  forms across four readers.
- test_typed_defaults_keep_initializer_complexity asserts names, count 3 and CCN 5.
- test_shared_parameter_names asserts Python/C++/Ruby/PHP names and counts.
- test_duplicated_parameter_lists_preserve_dollar_names asserts duplicate
  counts [2,2,1] and the literal '$a,$b,$c,$d,$e' key.
- test_parameter_outputs_and_warning_threshold observes CSV PARAM=3,
  Checkstyle '3 PARAM', -a2 warning selection, and HTML threshold cell 3.

Setup is literal valid source passed to the real analyzer; output tests use
real formatters/options and no parser mocks. reproduce.py asserts actual
facade/registry/functions/TS-state/FunctionInfo/test-helper file paths and
checks explicit expected counts for 21 fixed source files. Corrected passes
all 21; target fails11, raw fails4. Terminal copies retain every observation.

The coordinator independently ran full pytest on this exact product correction:
1721 passed, 7 skipped, 24 warnings in 4.03s. make exited0 after coverage pytest,
pycodestyle and pylint --exit-zero (score9.12). These results cover the current
accepted Python NS correctness assertions, including the #527 regression tests.
They precede the new trunk website integration; that consumer has separate proof.

## Unchanged defects and dependency

PHP source `<?php function plain($value, $tail) { return $value; } ?>` has two
parameters but is reported as1 on original baseline, target, raw and corrected.
All retain full_parameters=['$value $tail']; corrected normalized output retains
'$tail' instead of 'tail'. The single-parameter positive control does not prove
this unrelated comma-handling defect fixed. supplemental.json/logs retain it.

The generic function declaration in preexisting.json is absent on both target
and raw. It is not part of the identifier-name correction. Generic annotations
within recognized signatures are separately asserted and measured.

PR #535's original child 171079e6 adds first-token this detection to the type-state
introduced by PR #532. Actual onClick(this:HTMLElement,event:Event) counts are
2 on target/raw/corrected #532 and1 on the #535 child. This verifies the inherited
implementation dependency and the separate later promise. PR #532 needs no #535
prerequisite, and no backlog supplier agreement was invented.

## Performance and limits

Default target/initial equivalence is proved by exact public API observations,
identical broad CLI output hashes and the product diff containing only unused
default-path NS methods. Therefore default incremental comparisons also cover
default cumulative cost. This reasoning excludes -ENS, whose separate cumulative
measurement compares original2e21 directly to the corrected combined snapshot.

One central harness, fixed manifests, Python 3.9.11, warmups, calibrated batches
and 20 alternating pairs are retained. API measures 16 related JS-family inputs;
api-parameters measures 21 related inputs and reads public names/counts after
analysis. Broad CLI always analyzes the original four fixed source targets.

| Workload | Median change | Paired 95% interval | Verdict |
| --- | ---: | --- | --- |
| Raw affected API | +0.0481% | [-0.5466%,+0.3225%] | timing passes; code invalid |
| Raw broad CLI | -0.1495% | [-3.2614%,+5.5415%] | inconclusive |
| Corrected affected API | -0.4940% | [-1.0915%,+0.0035%] | passes |
| Corrected names/count consumers | +2.2310% | [+1.1198%,+2.8417%] | passes |
| Corrected broad CLI,1.2s | -0.6798% | [-3.7154%,+10.0648%] | inconclusive |
| Corrected broad CLI,8s | +3.1645% | [-4.6621%,+5.6704%] | inconclusive |
| Corrected broad CLI,quiet 40 pairs | -0.3562% | [-1.1537%,+2.3613%] | passes |
| Cumulative CLI -ENS,8s | -0.1271% | [-1.4399%,+0.4624%] | passes |

The longer ordinary CLI batch had medians 9.6683/9.9742 s and SD 1.0033/0.9528 s;
paired geometric change was +0.3713%. Its median alone does not establish a
strict below 3% result. No pair was filtered. After the implementation return,
root observed load 10.99/10.82/13.17 on 16 CPUs and the later ENS run's log-ratio
SD of 2.04% versus ordinary CLI's 10.99%. This changed condition justified one
prospectively fixed 40-pair ordinary run, calibrated to eight seconds, with every
pair retained and only its terminal interval judged. final-cli-decision.json
records that choice before launch; final-cli.py verifies unchanged code and corpus.
The complete terminal run passes: medians 7.31951/7.29343 s, SD 0.31302/0.42197 s,
41 CLI invocations per batch, paired geometric +0.58845%, upper +2.36131%.
All 40 pairs remain in corrected-cli-quiet.json; calibration targeted eight seconds
and actual warmed batches exceeded the source's one-second minimum. This resolves
the ordinary gate while preserving the earlier inconclusive verdicts.

The cumulative ENS run compares original 2e21 directly to the unchanged combined
corrected snapshot: medians 7.12313/7.11408 s, SD 0.18872/0.19012 s, 20 pairs,
39 CLI invocations per batch. Calibration targeted eight seconds; improved pilot
conditions yielded roughly seven-second warmed batches, above the source's
one-second minimum. Its paired upper bound is +0.46245%, so G6's timing passes.

Host: Territory.local, arm64, 16 CPUs/48 GiB, with unrelated VM/pytest/browser work.
Load moved from 16.12 to 27.83 before improving to 16.11 at the long launch;
the cumulative NS launch has a separate fresh observation. All owned proof is
serial. Raw invalid timing is retained without repeated attempts to pass.

One bounded profile per target/corrected snapshot checks byte-identical CLI
output and actual source paths. FunctionInfo.parameters used 0.00431/0.00422 s
over 1,636 calls. Whole profiled CLI time was about 0.460/0.466 s. A single profile
cannot attribute scheduling effects or satisfy the repeated timing gate.

Initial profile setup failed because snapshot profile.py uses Python 2 syntax
and shadowed stdlib profile. Launching cProfile from /private/tmp fixes that
specific setup error; original child stderr and terminal failure remain saved.
Earlier setup used unsupported tarfile.extractall(filter=...) on Python 3.9 and
one test selection named nonexistent testTypeScriptTypes.py; neither ran proof.
Corrected setup and actual complete suite selections are retained.

## Final acceptance

Independent refactor_532 returned REFACTOR COMPLETE with no edits or redundant
verification. Correction publication used agent-commit and a non-forced fork push.
Final CI run40466422315 passed all14 Ubuntu/Windows×Python3.8–3.14 jobs.
The proposed and landed merge tree eab2c48635db788e72ba142c6935d483e4a639ef
matches the validated anticipated tree. Its parents are current trunk40d89f7
and corrected head2a8af8a. Final head/base refresh preceded guarded hosted merge;
the contributor was thanked afterward. final-ci.json and hosted-publication.json
retain exact evidence. Ordinary broad CLI passes on the complete quiet run.
The original HTTP attempt lacked a runtime and remains recorded as observed:false.
Root then ran validate-current-base.py through Nix in an isolated worktree on
d8e8e9a, whose development requirements install Flask. It contains the exact
corrected product, 34 new tests and both authored tests. Full pytest1751/7 and
make exit0 pass. The real /analyze endpoint returns render=3, round=2, dollars=3
and destructuring=2, with asserted actual source paths. current-base-proof.json
records all commands and anticipated merge tree c7de325e738d18d493fe401a686fc53e3f7d4e51.
The profile-only upstream advance to40d89f7 changes no runtime/test/setup bytes;
current-base-refresh.json records reuse and the final anticipated tree above.
