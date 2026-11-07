# PR547 consumer dispositions

Trace commands:

```sh
node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/conclude-the-javascript-and-typescript-lexical-a' \
  --base HEAD \
  --term OPTIONAL_NAME --term generate_tokens --term TypeScriptReader --term TSXReader

node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/conclude-the-javascript-and-typescript-lexical-a' \
  --path lizard_languages/typescript.py \
  --path lizard_languages/tsx.py \
  --path test/test_languages/testOptionalOperators.py \
  --term OPTIONAL_NAME --term generate_tokens --term TypeScriptReader --term TSXReader
```

Records: consumer-trace.md (full HEAD), consumer-trace-product.md (path-scoped).

## Effect terms

| Term | Origin |
| --- | --- |
| `OPTIONAL_NAME` | shared optional-marker token alternative |
| `generate_tokens` | TypeScriptReader / TSXReader tokenization entry |
| `TypeScriptReader` | shared JS/TS reader owner |
| `TSXReader` | JSX/TSX tokenizer that now shares OPTIONAL_NAME |
| changed paths | `typescript.py`, `tsx.py`, `testOptionalOperators.py` |

## Production consumers

| File | Disposition |
| --- | --- |
| `lizard_languages/typescript.py` (`OPTIONAL_NAME` / `generate_tokens`) | run: owned change; unspaced ternary + optional member/params |
| `lizard_languages/tsx.py` (`TSXReader.generate_tokens` + `OPTIONAL_NAME`) | run: owned change; focused TSX/JSX + optional field/params |
| `lizard_languages/javascript.py` (`JavaScriptReader` → `TypeScriptReader`) | run: focused JS suite + `pick.js` / ternary controls |
| `lizard_languages/vue.py` (`VueReader` → `TypeScriptReader.generate_tokens`) | run: focused `testVue.py` |
| `lizard_languages/javascript_templates.py` (`template_tokens`) | reused: still receives addition string; no template rule change |
| `lizard_languages/js_style_regex_expression.py` | reused: decorator around generate_tokens unchanged |
| `lizard_core/file_analyzer.py` (`condition_counter`) | reused: still counts `?` tokens; tokenizer decides shapes |
| `lizard_languages/code_reader.py` | reused: generate_tokens protocol unchanged |
| Prior review manifests under `docs/reviews/fabio-malpezzi-20261027/pr5*` | excluded: historical evidence |

## Test consumers

| File | Disposition |
| --- | --- |
| `test/test_languages/testOptionalOperators.py` | run: new discriminating ternary/optional cases |
| `test/test_languages/testJavaScript*.py`, `testTypeScript*.py` | run: focused suite |
| `test/test_languages/testES6.py`, `testJSX.py`, `testTSX.py`, `testVue.py` | run: focused suite |

## Guidance

| File | Disposition |
| --- | --- |
| `.agents/skills/lizard-language-support/SKILL.md` | excluded: docs mention only |
| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` | excluded: planning authority |
| `docs/adrs/0000-use-adrs-accepted.md`, `0001-ubiquitous-language-accepted.md` | reused: terminology (reader, CCN, token) |
