# PR547 unspaced ternary vs optional markers review

Disposition: **merged** as `8567a4c7ee24029d76e54861defef01679238f34` at
corrected head `bf75e500ae1495dfbf39ad3eef0cf6fef5baaaee` after full suite/`make`,
fourteen-job CI, and exact-head merge. Independent refactor: none — already
clean. Thank-you:
https://github.com/terryyin/lizard/pull/547#issuecomment-6463154289.

Frozen contributor head: `2766557096daac3f8e55657e9c48ba685157635d` (OPEN;
CONFLICTING vs current master; historical 4-job checks SUCCESS). Current target
during review: `2e2adc4a76a3dc1e7d835e248f7cde223253e852` (includes merged
#546). Starting cumulative baseline:
`2e21e404c1ab9f4e0b49acc80816e816629d0670`. Discussion, reviews and inline
comments are empty. metadata.json retains the refreshed GitHub observation.

## Value

On current target, `analyze_file.analyze_source_code` reports CCN 1 for
`function pick` returning `x?y:0` in `.js`/`.ts` because
`(?:\w+\?(?![?.]))` keeps `x?` as one token. Correct metrics are CCN 2. In
`.tsx`/`.jsx`, optional field/params `name?: string` / `m(a?: number, b?: string)`
are counted as ternaries (CCN 3 for `m`); correct is CCN 1. Accurate CCN
benefits API users and warning thresholds. ADR-0001 governs CCN meaning;
ADR-0000 keeps human architectural ownership. PFE: share one `OPTIONAL_NAME`
rule across TS and TSX — no parallel counter.

## Adaptation

Raw head conflicts with current target. Adapted change extracts
`OPTIONAL_NAME = r"|(?:\w+\?(?=\s*(?:[:,)]|\Z)))"` so a `?` stays with its name
only where an optional marker can stand; `.ts`/`.js`/`.tsx`/`.jsx` share the
rule via `TypeScriptReader.generate_tokens` and `TSXReader.generate_tokens`.
Discriminating tests cover unspaced ternaries and optional members/params.
`?.` / `??` keep existing tokens. Correction+cleanup fit the ≤15-minute product
budget. literal-git-patches.json holds exact raw/adapted patches; raw.diff and
adapted.diff are DISPLAY ONLY.

Independent refactor: none — already clean.

## Proof

External root: `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr547`.
Central paired harness SHA256
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`.

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr547/bootstrap.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr547/reproduce.py
PIP_QUIET=1 nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_languages/testJSX.py test/test_languages/testTSX.py test/test_languages/testVue.py test/test_languages/testOptionalOperators.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr547/measure.py
PIP_QUIET=1 nix develop -c python -m pytest
PIP_QUIET=1 nix develop -c make
```

Target reproduction mismatches pick/ternary (`.js`/`.ts`) and optional
field/params (`.tsx`/`.jsx`) fixtures (red). Corrected matches all twenty
semantic inputs (green). Focused suite: 649 passed, 5 skipped. Full pytest:
2135 passed, 7 skipped. make: exit 0. Consumer dispositions:
consumer-trace-dispositions.md.

## Performance

Attempt1 prospectively chose 2.5s API / 4.0s CLI on the expanded corpus
(~1602 affected files after prior-PR inputs, bulk controls, and replicas) with
`PYTHONDONTWRITEBYTECODE=1` and cleared snapshot `__pycache__`. All six gates
pass (upper CI strictly &lt;3%):

| Comparison | Workload | Median change | Paired 95% CI | Gate |
| --- | --- | ---: | --- | --- |
| target→corrected | API | +0.588% | [−0.441%, +0.980%] | pass |
| target→corrected | names/counts | −0.037% | [−1.238%, +0.860%] | pass |
| target→corrected | broad CLI | +0.079% | [−0.691%, +1.897%] | pass |
| baseline→corrected | API | −8.715% | [−9.411%, −8.204%] | pass |
| baseline→corrected | names/counts | −8.919% | [−10.054%, −8.667%] | pass |
| baseline→corrected | broad CLI | +0.546% | [−1.215%, +1.171%] | pass |

## Merge

Published adapted product onto contributor head without force; guarded hosted
merge by exact head `bf75e500ae1495dfbf39ad3eef0cf6fef5baaaee`; required current
CI run https://github.com/terryyin/lizard/actions/runs/40648229253 succeeded;
merged as `8567a4c7ee24029d76e54861defef01679238f34`; thank-you posted.
