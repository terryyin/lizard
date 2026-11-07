# PR546 consumer dispositions

Trace commands:

```sh
node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/conclude-the-javascript-and-typescript-lexical-a' \
  --base HEAD \
  --term process_token --term add_condition --term conditions --term TypeScriptReader

node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/conclude-the-javascript-and-typescript-lexical-a' \
  --path lizard_languages/typescript.py \
  --path test/test_languages/testJavaScript.py \
  --term process_token --term add_condition --term conditions --term TypeScriptReader
```

Records: consumer-trace.md (full HEAD), consumer-trace-product.md (path-scoped).

## Effect terms

| Term | Origin |
| --- | --- |
| `process_token` | TypeScriptReader override that undoes keyword member conditions |
| `add_condition` | FileInfoBuilder / condition counter undo (`-1`) |
| `conditions` | reader combined condition set (catch/if/for/while/case/…) |
| `TypeScriptReader` | shared JS/TS/JSX/TSX/Vue reader owner |
| changed paths | `typescript.py`, `testJavaScript.py` |

## Production consumers

| File | Disposition |
| --- | --- |
| `lizard_languages/typescript.py` (`TypeScriptReader.process_token` / fast path) | run: owned change; focused TS + `load.ts` / members |
| `lizard_languages/javascript.py` (`JavaScriptReader` → `TypeScriptReader`) | run: focused JS suite + `load.js` |
| `lizard_languages/tsx.py` (`TSXReader` inherits `process_token` / `__call__`) | run: focused TSX/JSX + `load.jsx`/`.tsx` |
| `lizard_languages/vue.py` (`VueReader` inherits TypeScriptReader) | run: focused `testVue.py` |
| `lizard_core/file_analyzer.py` (`condition_counter`) | reused: still counts tokens; reader undoes members |
| `lizard_languages/code_reader.py` (`CodeReader.__call__` / `process_token` protocol) | reused: slow-path protocol; fast path allows `TypeScriptReader.process_token` |
| `lizard_languages/swift.py` (similar member undo pattern) | excluded: other language; pattern cite only |
| `lizard_ext/lizardignoreassert.py`, `lizardcomplextags.py`, `lizardmodified.py` | excluded: extension condition filters unchanged |
| Prior review manifests under `docs/reviews/fabio-malpezzi-20261027/pr5*` | excluded: historical evidence |

## Test consumers

| File | Disposition |
| --- | --- |
| `test/test_languages/testJavaScript.py` | run: new discriminating class |
| `test/test_languages/testTypeScript*.py` | run: focused suite |
| `test/test_languages/testES6.py`, `testJSX.py`, `testTSX.py`, `testVue.py` | run: focused suite |
| `test/test_languages/testTypeScriptReaderDispatch.py` | run: process_token dispatch path covered in focused suite |

## Guidance

| File | Disposition |
| --- | --- |
| `.agents/skills/lizard-language-support/SKILL.md` | excluded: docs mention only |
| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` | excluded: planning authority |
| `docs/adrs/0000-use-adrs-accepted.md`, `0001-ubiquitous-language-accepted.md` | reused: terminology (reader, CCN, condition) |
