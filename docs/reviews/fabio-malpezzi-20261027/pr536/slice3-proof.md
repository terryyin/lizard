# Formal slots and annotations — slice 3 proof

Base: `13c3403fc918209d1279018fd8cb5dad0405f144`, in the established
SEED-016 execution checkout. Product/test/input hashes are retained in
`slice3-source-hashes.json`. No integration with newer trunk was attempted.

## Changed ownership

`FunctionInfo.full_parameters` retains structural `{}` / `[]` spellings for
formal binding patterns. Its `parameter_count` includes those nameless slots;
`parameters` continues to expose established identifier matches only. This is
one shared representation, with no parser-local count or public placeholder
identifier. Existing nonempty names and trailing-comma exclusion are preserved.
`JavaScriptBindings` emits the structural spelling through `add_parameter`
and excludes a leading separator when a TypeScript receiver contributed no
formal slot. Named and anonymous signatures use the same existing owner.

The existing TypeScript annotation reader distinguishes a grouped return type
from function-type parameter parentheses; its following runtime arrow is
returned to the signature owner. Callback annotation arrows stay annotations.
CSV/checkstyle consume `parameter_count`. HTML serializes that property once
and uses it for both its cell and threshold class. Duplicate-list processing
continues to use identifier names, including its existing name-list threshold.
ADR-0000/0001 remain Accepted and unsuperseded; no exception or parser redesign.

## Public observations and source obligations

All new tests are in `test/test_languages/testJavaScriptFormalSlots.py`
(132 lines). Setup supplies only source, a real temporary file or threshold
settings; no parsing logic or analyzer result is mocked.

| Original trigger / expected result | Observing assertion / actual result |
| --- | --- |
| G1: named/anonymous `({}, [], tail,)` has count3, names `tail` | `test_named_and_anonymous_empty_patterns_occupy_slots`: exact names/counts across JS/TS/JSX/TSX/Vue/cjs/mjs |
| Receivers and trailing commas add no slot | `test_receiver_and_trailing_commas_do_not_invent_slots`: receiver-only0, two empty patterns2, one identifier1, empty signature0 in TS/TSX |
| Callback annotation creates no runtime phantom | `test_callback_type_annotation_has_no_runtime_function`: one anonymous count4 with `number,$,cb,tail`, then the actual following function, in TS/TSX |
| G2: grouped function-return type retains outer arrow | `test_function_return_annotation_keeps_outer_arrow`: inner `n` count1, outer `x,y` count2, then following `after`, in TS/TSX |
| Generic and object-return annotations preserve formals | `test_generic_and_object_return_annotations_preserve_formals`: named/anonymous generics and object return each retain exact two names/count2 in TS/TSX |
| Nested and ternary defaults retain CCN ownership | `test_nested_and_ternary_defaults_keep_complexity_owners`: names preserved; CCN5,4,2 for nested-default arrow, ternary arrow and outer function |
| G3: count outputs and warnings use formal slots | `test_empty_slots_reach_count_outputs_and_warning_threshold`: actual analyzed arrow has CSV3, checkstyle `3 PARAM`, warning above2 and highlighted HTML3 |
| Empty patterns introduce no duplicate-list key | `test_empty_slots_do_not_add_duplicate_parameter_keys`: count7 with five names; duplicate counts2/2 and sole key `a,b,c,d,e` |
| Actual CLI sees formal count and threshold | `test_cli_counts_empty_slots_for_parameter_warnings`: real file, PARAM3 row, `parameter_count > 2` warning and exit1 |
| Website JSON uses the same formal count | `test_website_json_counts_empty_slots`: actual `/analyze` JS/TS response has one anonymous function with count3 |

## Commands and terminal results

Red before product edits:

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScriptFormalSlots.py -q > /tmp/seed016-slice3-red.log 2>&1
```

Terminal exit1:16 failed,5 passed. Empty slots reported1 rather than3;
the grouped-return case omitted the outer arrow. The already-correct typed,
generic/object-return and CCN cases passed. `slice3-red-excerpt.log` retains
the first concrete mismatch and all failing selections; the full raw log is
retained at its literal command path.

The first product attempt left three failures: two receiver+empty-pattern
cases exposed the legacy leading comma before a nameless first formal, and
HTML raised `UndefinedError` because its dictionary lacked the new property.
The binding separator and HTML serialization changes above corrected those
causes. No fixture or expected result was weakened.

```sh
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScriptFormalSlots.py test/test_languages/testJavaScriptArrowParameters.py test/test_languages/testJavaScriptParameterNames.py test/test_languages/testTypeScriptThisParameters.py -q > /tmp/seed016-slice3-focused.log 2>&1
```

Terminal exit0:104 passed in0.28s. This includes all21 new formal-slot cases
plus prior anonymous-capture, shared identifier and TypeScript receiver proof.

```sh
PIP_QUIET=1 nix develop -c python -m pytest -q > /tmp/seed016-slice3-full-pytest.log 2>&1
```

Terminal exit0:1944 passed,7 skipped in11.92s. All reached reader, core,
extension, output, warning, API/CLI and website suites pass. Existing large
tests are byte-identical. No full `make` or timing run was performed here.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr536/reproduce.py "$PWD" /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/all-inputs.json > /tmp/seed016-slice3-candidate.jsonl 2> /tmp/seed016-slice3-reproducer-setup.log
```

Terminal exit1: all65 API cases and the actual CLI phase completed;63 rows
pass, including all34 accepted PR532/535 controls. Actual checkout facade,
reader and core imports are verified before and after. `slice3-candidate.jsonl`
retains counts, names, metrics and CLI results. Exactly these source exclusions
fail: member-assignment half of `arrow-member.js` remains count0 (wrapper2),
and `arrow-abandoned.js` outer CCN remains2 rather than3. Neither expectation
was altered. The cache inputs/corpus are the valid setup established in slice2.

## Consumer trace and verification ownership

```sh
PIP_QUIET=1 nix develop -c node .agents/skills/dough-execute-plan/scripts/consumer-trace.mjs --checkout "$PWD" --base 13c3403fc918209d1279018fd8cb5dad0405f144 --term parameter_count --term full_parameters --term parameters --term _create_dict --term _function_type_annotation --term _after_function_type_params --term function_info --term csvoutput --term checkstyleoutput --term htmloutput --term javascript_bindings --term typescript_declarations > /tmp/seed016-slice3-consumers-full.md 2> /tmp/seed016-slice3-consumers-setup.log
```

[Live consumer trace](slice3-consumers.md) and its two linked historical
companions retain all282 generated rows with a disposition; exact row-content
preservation was checked. Inspected runtime paths include FunctionInfo creation,
other-language token accumulation, CLI/output scheme, extension registration,
duplicate names, website serialization and unchanged frontend `fn[field]`
rendering. Full pytest covers other-language counts/names and legacy outputs;
the new public assertions above observe the actual new slot behavior. Other
readers retain ordinary parameter strings rather than emitting JS structural
slots. Browser rendering or deployed-site behavior is not claimed.

Before full pytest, localhost had16 logical cores, load3.21/3.86/4.34 and61%
free memory. Prior104 tests took0.28s and prior expanded572 took2.28s; no
competing owned heavy proof was known. Chosen proof was one serial full suite;
its terminal result was observed. Nix's existing platformdirs resolver warning
did not prevent any passing verification. No subprocess or watch remains owned
by this implementation. Performance and final make/hosted/contributor delivery
remain slices4–5. G1–G3 have no remaining mismatch in the promised examples.

Independent `slots_refactor` returned `none — already clean` and
`## REFACTOR COMPLETE`; no product, test or evidence edits invalidated proof.
Its three concurrent Nix text-read entries all exited0, but one shell hook
reported an ensurepip error. This violated the serial shell-hook requirement.
After all commands ended, the coordinator restored checkout readiness serially:
`PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScriptFormalSlots.py -q`
exited0 with21 passed in0.14s. Only the pre-existing platformdirs warning
remained; no ensurepip error occurred. Source and locked metadata were unchanged.
