# PR545 consumer dispositions

Trace commands:

```sh
node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/conclude-the-javascript-and-typescript-lexical-a' \
  --base HEAD \
  --term interpolation_code --term process_token --term template_tokens \
  --term generate_tokens --term conditions

node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/conclude-the-javascript-and-typescript-lexical-a' \
  --path lizard_languages/javascript_templates.py \
  --path lizard_languages/typescript.py \
  --term interpolation_code --term process_token --term template_tokens \
  --term TypeScriptReader
```

Records: consumer-trace.md (full HEAD), consumer-trace-product.md (path-scoped).

## Effect terms

| Term | Origin |
| --- | --- |
| `interpolation_code` | new helper on the template scanner |
| `process_token` | TypeScriptReader override that counts interpolation conditions |
| `template_tokens` | existing JS template tokenizer (unchanged call sites) |
| `TypeScriptReader` | shared reader for `.js`/`.ts`/`.jsx`/`.tsx` and Vue script |
| changed paths | `javascript_templates.py`, `typescript.py`, `testTypeScript.py` |

## Production consumers

| File | Disposition |
| --- | --- |
| `lizard_languages/javascript.py` (`JavaScriptReader` → `TypeScriptReader`) | run: focused JS suite + story `label.js` |
| `lizard_languages/typescript.py` (`TypeScriptReader.process_token` / `generate_tokens`) | run: owned change; focused TS + `label.ts` |
| `lizard_languages/tsx.py` (`TSXReader` inherits `process_token`; atomic templates) | run: focused TSX/JSX + `label.jsx`/`.tsx` |
| `lizard_languages/vue.py` (`VueReader` uses `TypeScriptReader.generate_tokens` / inherits reader) | run: focused `testVue.py` |
| `lizard_languages/javascript_templates.py` (`interpolation_code` / `template_tokens`) | run: owned helper; template nesting/literal controls |
| `lizard_languages/code_reader.py` (`CodeReader.__call__` / `process_token` protocol) | reused: generic protocol; TS override returns False so states still see opaque tokens |
| `lizard_core/file_analyzer.py` (`condition_counter`) | reused: still counts only whole tokens; interpolation conditions added in reader |
| `lizard_languages/{go,kotlin,php,...}.py` `generate_tokens` name hits | excluded: unrelated readers; no `interpolation_code` |
| Prior review manifests/logs under `docs/reviews/fabio-malpezzi-20261027/pr52*` | excluded: historical evidence, not live product boundary |

## Test consumers

| File | Disposition |
| --- | --- |
| `test/test_languages/testTypeScript.py` | run: new `Test_conditions_in_template_interpolations` |
| `test/test_languages/testJavaScript*.py` | run: focused suite |
| `test/test_languages/testTypeScript*.py` | run: focused suite |
| `test/test_languages/testES6.py`, `testJSX.py`, `testTSX.py`, `testVue.py` | run: focused suite |
| `test/test_languages/testTypeScriptReaderDispatch.py` | run: process_token dispatch path covered in focused suite |
| Other-language `generate_tokens` / `conditions` string hits | excluded: no shared interpolation path |

## Guidance

| File | Disposition |
| --- | --- |
| `.agents/skills/lizard-language-support/SKILL.md` | excluded: docs mention only |
| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` | excluded: planning authority |
| `docs/adrs/0000-use-adrs-accepted.md`, `0001-ubiquitous-language-accepted.md` | reused: terminology (reader, CCN, condition, token) |

## Slice-2 hot-path trim (attempt10)

`TypeScriptReader.__call__` restored the attempt8-style `after_interpolation` /
bound `scan_atomic` checks so ordinary JS/TS tokens avoid a per-token
`_count_conditions_from_token` method call. `process_token` still uses that
helper; interpolation keyword-member undo (#546) remains inside
`_count_interpolation_conditions`. Consumers unchanged: same JS/TS/TSX/Vue
paths; re-run focused suite after trim.

## Deferred residual

Keyword-member residual inside interpolations is a regression guard only after
#546 (not reopened here).
