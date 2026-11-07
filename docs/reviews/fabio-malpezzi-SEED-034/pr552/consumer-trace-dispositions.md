# PR552 consumer dispositions

Trace commands:

```sh
node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/process-fabiomalpezzi-s-open-prs' \
  --base 0de55d6f9ff122af298485744e3a8e1131af6fd5 \
  --term _consume_generic_type_params --term JavaScriptFunctionsMixin \
  --term TypeScriptDeclarationsMixin --term _function

node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/process-fabiomalpezzi-s-open-prs' \
  --base 0de55d6f9ff122af298485744e3a8e1131af6fd5 \
  --path lizard_languages/javascript_functions.py \
  --path lizard_languages/typescript_declarations.py \
  --path test/test_languages/testTypeScriptFunctions.py \
  --term _consume_generic_type_params --term JavaScriptFunctionsMixin \
  --term TypeScriptDeclarationsMixin --term _function
```

Records: consumer-trace.md (full base+WT), consumer-trace-product.md (path-scoped).

## Effect terms

| Term | Origin |
| --- | --- |
| `_consume_generic_type_params` | shared helper; optional `resume` so `function name<T>(...)` returns to `_function` |
| `JavaScriptFunctionsMixin` | `_function` call site passes `self._function` as resume |
| `TypeScriptDeclarationsMixin` | owner of `_consume_generic_type_params` |
| `_function` | resume state after consuming declaration type parameters |
| changed paths | `javascript_functions.py`, `typescript_declarations.py`, `testTypeScriptFunctions.py` |

## Production consumers

| File | Disposition |
| --- | --- |
| `lizard_languages/javascript_functions.py` | run: owned `_function` call site; resume into `_function` after `<...>` |
| `lizard_languages/typescript_declarations.py` | run: owned helper signature; default resume remains `_state_global` |
| `lizard_languages/javascript_objects.py` | reused: still calls `_consume_generic_type_params()` with default resume (methods/arrows) |
| `lizard_languages/typescript_states.py` | reused: still calls `_consume_generic_type_params()` with default resume |
| `lizard_languages/typescript.py` / `tsx.py` / `javascript.py` / `vue.py` | run: focused JS/TS suite + claim fixtures (readers compose the mixins) |

## Test consumers

| File | Disposition |
| --- | --- |
| `test/test_languages/testTypeScriptFunctions.py` | run: new `test_generic_function_declaration`; existing generic arrow controls |
| `test/test_languages/testJavaScript*.py`, `testTypeScript*.py` | run: focused suite |
| `test/test_languages/testES6.py`, `testJSX.py`, `testTSX.py`, `testVue.py`, `testOptionalOperators.py`, `testJavaScriptNestedStructures.py` | run: focused suite |

## Guidance

| File | Disposition |
| --- | --- |
| `.agents/skills/lizard-language-support/SKILL.md` | excluded: docs mention only |
| `.planning/slice-plans/022-fabio-malpezzi-open-prs/PLAN.md` | excluded: planning authority |
| `docs/adrs/0000-use-adrs-accepted.md`, `0001-ubiquitous-language-accepted.md` | reused: terminology (reader, function boundaries) |
| Prior review evidence under `docs/reviews/fabio-malpezzi-*` | excluded: historical evidence |
