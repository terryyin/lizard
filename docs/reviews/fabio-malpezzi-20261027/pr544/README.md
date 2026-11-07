# PR544 JavaScript line-comment termination review

Disposition: **merged** as `35b454b778239846de4b467715717f67ac4bc6c3` at
corrected head `77902f74186212f02abadca5d97f9699ba70e980` after independent
refactor (`none — already clean`), full suite/`make`, fourteen-job CI, and
exact-head merge. Thank-you:
https://github.com/terryyin/lizard/pull/544#issuecomment-6456484491.

Frozen contributor head: `3ac7e4eaf61e898836fded88ab715b37f9fedbbf` (OPEN;
hosted checks SUCCESS on the historical 4-job matrix). Reported base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`. Current target after
`git fetch origin master`: `aa5f40638b23aa54d4724c9429c1ead2183013fd`
(product sources unchanged by the Take commit). Starting cumulative baseline:
`2e21e404c1ab9f4e0b49acc80816e816629d0670`. Discussion, reviews and inline
comments are empty. metadata.json retains the refreshed GitHub observation.
required-workflow.yml copies the current target workflow when present.

## Value

On current target, `analyze_file.analyze_source_code` reports CCN 3 / NLOC 7 for
the story `b` fixture across `.js`/`.ts`/`.jsx`/`.tsx` because shared
`token_pattern` continues `//` after `\`, as C does. ECMAScript line comments
end at the line terminator; the next line's `||` must count. Correct metrics
are CCN 4 / NLOC 8. Accurate CCN/NLOC benefit API users and warning thresholds.
ADR-0001 governs CCN/NLOC meanings; ADR-0000 keeps human architectural ownership.
PFE: reuse `TypeScriptReader` / `CCppCommentsMixin` / `javascript_templates`
owners — no parallel parser.

## Adaptation

Raw head conflicts with current target (`JS_IDENTIFIER` replaced `|(?:\$\w+)`).
Adapted change keeps `JS_IDENTIFIER`, extracts shared `LINE_COMMENT`, and adds
it to TypeScript and TSX `generate_tokens` additions so it matches before the
shared continuing `\/\/` pattern. Shared `token_pattern` / C/C++ continuation
are untouched. The discriminating test uses the PR-body/story fixture (CCN 4 /
NLOC 8), not the PR's shorter CCN 3 / NLOC 7 sample.

Correction+cleanup estimated at ~9 minutes before edits; no upward revision.
literal-git-patches.json holds exact raw and adapted patches; raw.diff and
adapted.diff are DISPLAY ONLY.

## Proof

External root: `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr544`.
Central paired harness SHA256
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr544/bootstrap.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr544/reproduce.py
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py
PIP_QUIET=1 nix develop -c python -m pytest 'test/testTokenizer.py::Test_generate_token_for_comments::test_cpp_style_comment_with_multiple_lines' test/test_languages/testCAndCPP.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr544/measure.py
PIP_QUIET=1 nix develop -c python -m pytest
PIP_QUIET=1 nix develop -c make
```

Target reproduction mismatches the five `b-*` fixtures (red). Corrected matches
all twelve semantic inputs (green), including C/C++ continuation controls at
CCN 2. Focused suite: 610 passed, 5 skipped. Tokenizer + C/C++: 112 passed.
Full pytest: 2128 passed, 7 skipped. make: exit 0.

## Performance

Attempt1 (1.2s batches, 12 affected files, load ~11) failed incremental API
(~7% upper CI) and is retained under attempt1/ as inconclusive.
Attempt2 prospectively chose 2.5s API / 4.0s CLI, 76-file affected corpus
(inputs + prior 535/536 + bulk controls), cleared `__pycache__`,
`PYTHONDONTWRITEBYTECODE=1`. All six gates pass (upper CI strictly <3%):

| Comparison | Workload | Median change | Paired 95% CI | Gate |
| --- | --- | ---: | --- | --- |
| target→corrected | API | +0.778% | [+0.435%, +1.013%] | pass |
| target→corrected | names/counts | +0.640% | [+0.299%, +1.185%] | pass |
| target→corrected | broad CLI | −0.597% | [−1.379%, +0.682%] | pass |
| baseline→corrected | API | −10.230% | [−10.530%, −9.321%] | pass |
| baseline→corrected | names/counts | −10.476% | [−10.645%, −9.831%] | pass |
| baseline→corrected | broad CLI | +0.240% | [−5.459%, +1.622%] | pass |

## Consumers

Installed consumer-trace against the uncommitted product diff. JS/TS/JSX/TSX/Vue
readers share `TypeScriptReader.generate_tokens` or TSX override; both receive
`LINE_COMMENT`. C/C++ and other `CCppCommentsMixin` languages excluded (shared
`until_end` continuation unchanged). Dispositions:
consumer-trace-dispositions.md.
