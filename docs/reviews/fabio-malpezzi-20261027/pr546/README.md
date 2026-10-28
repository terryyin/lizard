# PR546 keyword member names review

Disposition: **merged** as `559b8c8da2b4e230269d9856969087b08bc241b9` at
corrected head `395ae04d85f9a2cb181f1022340ce424fe549eb0` after full suite/`make`,
fourteen-job CI, and exact-head merge. A post-change refactor that called
`process_token` on every fast-path token failed remasure (attempt2 retained) and
was reverted to the inlined undo before publish. Thank-you:
https://github.com/terryyin/lizard/pull/546#issuecomment-6461965881.

Frozen contributor head: `a671632141127fca9b2785fdbb0c675d45ae9042` (OPEN;
CONFLICTING vs current master; historical 4-job checks SUCCESS). Current target
during review: `f0887eca8ab11c5e4d743a58424e221699ae1de9` (includes merged
#544). Starting cumulative baseline:
`2e21e404c1ab9f4e0b49acc80816e816629d0670`. Discussion, reviews and inline
comments are empty. metadata.json retains the refreshed GitHub observation.
Do not depend on #545 product landing.

## Value

On current target, `analyze_file.analyze_source_code` reports CCN 2 for
`function load` returning `p.then(parse).catch(() => null)` because the
condition counter treats every `catch`/`if`/`for`/`while`/`case` token as a
condition, including member names after `.` or `?.`. Correct metrics are CCN 1.
Genuine control-flow constructs still increment. Accurate CCN benefits API users
and warning thresholds. ADR-0001 governs CCN meaning; ADR-0000 keeps human
architectural ownership. PFE: extend existing `TypeScriptReader` condition
handling — no parallel counter.

## Adaptation

Raw head conflicts with current target (`LINE_COMMENT` / `JS_IDENTIFIER` /
fast-path `__call__`). Adapted change keeps the raw undo
(`add_condition(-1)` when the previous token is `.` or `?.` and the token is in
`self.conditions`) and, learning from #545, allows
`TypeScriptReader.process_token` on the fast path so the cheap previous-token
check does not force `CodeReader.__call__`. Discriminating tests cover the four
extensions; genuine catch/if/for/while/case controls remain CCN ≥ 2.
Correction+cleanup fit the ≤15-minute product budget. literal-git-patches.json
holds exact raw/adapted patches; raw.diff and adapted.diff are DISPLAY ONLY.

Deferred residual (keyword member inside interpolation after #545+#546) left
deferred: #545 is unmerged; no ≤15-minute coherent in-cycle fix without that
landing.

Independent refactor: none — already clean.

## Proof

External root: `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr546`.
Central paired harness SHA256
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr546/bootstrap.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr546/reproduce.py
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr546/measure.py
PIP_QUIET=1 nix develop -c python -m pytest
PIP_QUIET=1 nix develop -c make
```

Target reproduction mismatches the eight `load-*` / `members-*` fixtures
(red: load CCN 2). Corrected matches all sixteen semantic inputs (green),
including genuine control-flow controls. Focused suite: 611 passed, 5 skipped.
Full pytest: 2129 passed, 7 skipped. make: exit 0. Consumer dispositions:
consumer-trace-dispositions.md.

## Performance

Attempt1 prospectively chose 2.5s API / 4.0s CLI on the expanded corpus
(~1394 affected files after prior-PR inputs, bulk controls, and replicas) with
`PYTHONDONTWRITEBYTECODE=1` and cleared snapshot `__pycache__`. All six gates
pass (upper CI strictly &lt;3%):

| Comparison | Workload | Median change | Paired 95% CI | Gate |
| --- | --- | ---: | --- | --- |
| target→corrected | API | +0.643% | [−0.015%, +0.973%] | pass |
| target→corrected | names/counts | −0.228% | [−0.046%, +1.105%] | pass |
| target→corrected | broad CLI | +0.073% | [−1.639%, +0.844%] | pass |
| baseline→corrected | API | −8.834% | [−9.886%, −8.474%] | pass |
| baseline→corrected | names/counts | −8.164% | [−8.667%, −7.281%] | pass |
| baseline→corrected | broad CLI | +0.341% | [−0.940%, +2.192%] | pass |

## Coordinator remaining

Publish adapted product onto contributor head without force; guard hosted merge
by exact head; required current CI; merge; thank-you comment. Do not land #545
with this PR.
