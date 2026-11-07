# PR544 consumer dispositions

Trace commands:

```sh
node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/conclude-the-javascript-and-typescript-lexical-a' \
  --base HEAD \
  --term LINE_COMMENT --term CCppCommentsMixin --term template_tokens --term generate_tokens

node '.claude/skills/dough-execute-plan/scripts/consumer-trace.mjs' \
  --checkout '/Users/terryyin/git/lizard/.worktrees/conclude-the-javascript-and-typescript-lexical-a' \
  --path lizard_languages/javascript_templates.py \
  --path lizard_languages/typescript.py \
  --path lizard_languages/tsx.py \
  --term LINE_COMMENT --term template_tokens --term CCppCommentsMixin
```

Records: consumer-trace.md (full HEAD), consumer-trace-product.md (path-scoped).

## Effect terms

| Term | Origin |
| --- | --- |
| `LINE_COMMENT` | supplied / new shared constant |
| `template_tokens` | supplied / JS template tokenizer |
| `CCppCommentsMixin` | supplied / comment-text helper (unchanged behavior) |
| `generate_tokens` | supplied / reader token entry |
| changed paths | `javascript_templates.py`, `typescript.py`, `tsx.py`, `testJavaScript.py` |

## Production consumers

| File | Disposition |
| --- | --- |
| `lizard_languages/javascript.py` (`JavaScriptReader` → `TypeScriptReader`) | run: focused JS suite + story fixture `.js` |
| `lizard_languages/typescript.py` (`TypeScriptReader.generate_tokens`) | run: owned change; focused TS + story fixture `.ts` |
| `lizard_languages/tsx.py` (`TSXReader.generate_tokens`) | run: owned change; focused TSX/JSX + story fixtures |
| `lizard_languages/vue.py` (`VueReader` uses `TypeScriptReader.generate_tokens`) | run: focused `testVue.py` |
| `lizard_languages/javascript_templates.py` (`template_tokens` / opaque line comments) | run: owned extract; `testTypeScript.py` template backslash comment |
| `lizard_languages/token_pattern.py` (shared `\/\/` + `until_end`) | reused: unchanged; JS overrides via addition |
| `lizard_languages/clike.py` (`CCppCommentsMixin` / `CLikeReader`) | excluded: mixin only strips comment text; continuation unchanged |
| `lizard_languages/{go,kotlin,php,plsql,rust,scala,solidity,swift,tnsdl,zig}.py` | excluded: `CCppCommentsMixin` import only; no JS addition |
| Prior review manifests/logs under `docs/reviews/fabio-malpezzi-20261027/pr52*` | excluded: historical evidence, not live product boundary |

## Test consumers

| File | Disposition |
| --- | --- |
| `test/test_languages/testJavaScript.py` | run: new discriminating class |
| `test/test_languages/testTypeScript*.py` | run: focused suite |
| `test/test_languages/testES6.py`, `testJSX.py`, `testTSX.py`, `testVue.py` | run: focused suite |
| `test/testTokenizer.py::Test_generate_token_for_comments` | run: C-style `//a\\\nb` continuation control |
| `test/test_languages/testCAndCPP.py` | run: C/C++ suite control (112 with tokenizer) |
| `test/test_languages/testPerl.py`, `testPLSQL.py` unrelated LINE_COMMENT name hits | excluded: string match only |

## Guidance

| File | Disposition |
| --- | --- |
| `.agents/skills/lizard-language-support/SKILL.md` | excluded: docs mention only |
| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` | excluded: planning authority |
| `docs/adrs/0000-use-adrs-accepted.md`, `0001-ubiquitous-language-accepted.md` | reused: terminology (reader, CCN, NLOC, condition) |
