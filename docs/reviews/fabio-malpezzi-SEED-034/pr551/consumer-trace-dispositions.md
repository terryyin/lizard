# PR551 consumer dispositions

Trace commands:

```sh
node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/process-fabiomalpezzi-s-open-prs' \
  --base 4a05e4056cebb2edd75cb02b7a43e911369f4ce5 \
  --term _end_field_before_modifier --term ends_expression \
  --term JavaScriptObjectsMixin --term TypeScriptStates

node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/process-fabiomalpezzi-s-open-prs' \
  --path lizard_languages/javascript_objects.py \
  --path lizard_languages/typescript_states.py \
  --path test/test_languages/testJavaScriptMembers.py \
  --path test/test_languages/testTypeScriptFields.py \
  --term _end_field_before_modifier --term ends_expression \
  --term JavaScriptObjectsMixin --term TypeScriptStates
```

Records: consumer-trace.md (full base+WT), consumer-trace-product.md (path-scoped).

## Effect terms

| Term | Origin |
| --- | --- |
| `_end_field_before_modifier` | new ASI helper ending an open class field before a line-start modifier |
| `ends_expression` | existing ASI predicate reused by the helper |
| `JavaScriptObjectsMixin` | class/object member owner (`get`/`set` path) |
| `TypeScriptStates` | shared JS/TS state machine (`async`/`static` path) |
| changed paths | `javascript_objects.py`, `typescript_states.py`, member/field tests |

## Production consumers

| File | Disposition |
| --- | --- |
| `lizard_languages/javascript_objects.py` | run: owned change; `get`/`set` after semicolon-less fields |
| `lizard_languages/typescript_states.py` | run: owned change; `async`/`static` after semicolon-less fields |
| `lizard_languages/typescript.py` (`TypeScriptStates`) | run: focused JS/TS suite + claim fixtures |
| `lizard_languages/tsx.py` (`TypeScriptStates`) | run: focused `testTSX.py` / `testJSX.py` |
| `lizard_languages/javascript.py` (`JavaScriptReader` → `TypeScriptReader`) | run: focused JS suite |
| `lizard_languages/vue.py` | run: focused `testVue.py` |
| `lizard_languages/javascript_functions.py` (`ends_expression`) | reused: helper still owns ASI predicate; call sites unchanged |
| `lizard_languages/typescript_declarations.py` / `typescript_signatures.py` | reused: still import `ends_expression`; no call-site change |
| `lizard_languages/ocaml.py` (`_ends_expression`) | excluded: unrelated local name |
| Prior review manifests under `docs/reviews/fabio-malpezzi-20261027/pr5*` | excluded: historical evidence |

## Test consumers

| File | Disposition |
| --- | --- |
| `test/test_languages/testJavaScriptMembers.py` | run: new discriminating modifier-after-field cases + multiline control |
| `test/test_languages/testTypeScriptFields.py` | run: new TS modifier-after-field case |
| `test/test_languages/testJavaScript*.py`, `testTypeScript*.py` | run: focused suite |
| `test/test_languages/testES6.py`, `testJSX.py`, `testTSX.py`, `testVue.py`, `testOptionalOperators.py`, `testJavaScriptNestedStructures.py` | run: focused suite |

## Guidance

| File | Disposition |
| --- | --- |
| `.agents/skills/lizard-language-support/SKILL.md` | excluded: docs mention only |
| `.planning/slice-plans/022-fabio-malpezzi-open-prs/PLAN.md` | excluded: planning authority |
| `docs/adrs/0000-use-adrs-accepted.md`, `0001-ubiquitous-language-accepted.md` | reused: terminology (reader, function boundaries) |
