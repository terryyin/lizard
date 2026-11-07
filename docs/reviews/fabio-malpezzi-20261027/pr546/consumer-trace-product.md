# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `lizard_languages/typescript.py` | changed path |
| `typescript.py` | basename of `lizard_languages/typescript.py` |
| `test/test_languages/testJavaScript.py` | changed path |
| `testJavaScript.py` | basename of `test/test_languages/testJavaScript.py` |
| `process_token` | supplied |
| `add_condition` | supplied |
| `conditions` | supplied |
| `TypeScriptReader` | supplied |

## Scope

Whole checkout at `d10f498f7f93`, including uncommitted and untracked files: 1549 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: `lizard_languages/typescript.py`, `test/test_languages/testJavaScript.py`.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `docs/reviews/fabio-malpezzi-20261027/controls/manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 99: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/frozen-review-set.json` | `conditions` | 154: `"headBranch": "pr/js-conditions-in-template-interpolations",` | |
| `docs/reviews/fabio-malpezzi-20261027/manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 99: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 99: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/proposed-patch.json` | `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `conditions` | 12: `"filename": "test/test_languages/testJavaScript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/review-input.json` | `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 12: `"body": "Supersedes #504. Since d90c086 (#505) a regex literal is recognized before\nstrings, which fixes most of what #504 addressed: of its 23 tests, 20 pass…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate-isolated.log` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 17: `test/test_languages/testJavaScript.py ........................           [ 25%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate.log` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 12: `../../../../.cache/lizard-pr-reviews-20261027/pr531-readonly/test/test_languages/testJavaScript.py . [ 17%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 101: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/review-input.json` | `conditions` | 7: `"dependencies": [{"number": 533, "head": "15423852c04aed302f684395fe9522251d88a2f2", "body": "Depends on #531.\n\n### The problem\n\nAn arrow function without …` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/affected-manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 113: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/cli-profile.json` | `process_token` | 1: `{"output_hashes":{"target":"357e5f2b950229ae217d3510faadc8dd561358f16d3da4bcdbb7163e0a5b287c","corrected":"357e5f2b950229ae217d3510faadc8dd561358f16d3da4bcdbb7…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumers-manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 118: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/corrected/test/test_languages/testTypeScript.…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/corrected/test/test_languages/testTypeScript.…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part1` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 71: `test/test_languages/testJavaScript.py ........................           [ 50%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part2` | `lizard_languages/typescript.py`, `typescript.py` | 11: `lizard_languages/typescript.py                     55      1    98%   39` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part3` | `lizard_languages/typescript.py`, `typescript.py`, `conditions` | 20: `lizard_languages/typescript.py:20:0: R0205: [1;3;35mClass 'Tokenizer' inherits from object, can be safely removed from bases in python3[0m ([1;3;35museless-…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-proof.json` | `lizard_languages/typescript.py`, `typescript.py` | 9: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-pytest.log` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 70: `test/test_languages/testJavaScript.py ........................           [ 50%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/dependency-535-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/dependency-535/test/test_languages/testTypeSc…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/final-cli-decision.json` | `conditions` | 31: `"earlier_runs": "Preserved without relabeling; improved host conditions justify this run."` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/final-cli.py` | `lizard_languages/typescript.py`, `typescript.py`, `conditions` | 15: `'lizard_languages/typescript.py', 'lizard_languages/tsx.py']` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/finish-proof.py` | `lizard_languages/typescript.py`, `typescript.py` | 35: `'lizard_languages/javascript_functions.py', 'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/observe-website.py` | `lizard_languages/typescript.py`, `typescript.py` | 52: `'lizard_languages/typescript.py', 'lizard_languages/tsx.py'):` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/prepare-proof.py` | `lizard_languages/typescript.py`, `typescript.py` | 15: `'lizard_languages/typescript.py', 'lizard_languages/tsx.py']` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/raw-target/test/test_languages/testTypeScript…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-preexisting.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/raw-target/test/test_languages/testTypeScript…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/raw-target/test/test_languages/testTypeScript…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/snapshot-hashes.json` | `lizard_languages/typescript.py`, `typescript.py` | 6: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/target/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-preexisting.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/target/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/target/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/validate-current-base.py` | `lizard_languages/typescript.py`, `typescript.py` | 18: `'lizard_languages/typescript.py', 'lizard_languages/tsx.py']` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/website-consumer.json` | `lizard_languages/typescript.py`, `typescript.py` | 9: `"lizard_languages/typescript.py": "74146767409186ca9cadcc0175f1e72ab58e089c10999553fee4d0fe11f9784f",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"revision": "2e21e404c1ab9f4e0b49acc80816e816629d0670", "target": "7ac449f59b3f155fd159ce9aad6794a422045632", "head": "15423852c04aed302f684395fe9522251d88a2f…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/review-input.json` | `conditions` | 9: `"body": "Depends on #531.\n\n### The problem\n\nAn arrow function without braces ends at the end of its line, but only when\nthe first token of the next line r…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/affected-manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 126: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumers-manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 131: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part1` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 71: `test/test_languages/testJavaScript.py ........................           [ 49%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part2` | `lizard_languages/typescript.py`, `typescript.py` | 13: `lizard_languages/typescript.py                     55      1    98%   39` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part3` | `lizard_languages/typescript.py`, `typescript.py`, `conditions` | 22: `lizard_languages/typescript.py:20:0: R0205: [1;3;35mClass 'Tokenizer' inherits from object, can be safely removed from bases in python3[0m ([1;3;35museless-…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-pytest.log` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 70: `test/test_languages/testJavaScript.py ........................           [ 49%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/finish-proof.py` | `TypeScriptReader` | 19: `'--term', 'parameter_count', '--term', 'TypeScriptReader',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/focused-command.json` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 7: `"test/test_languages/testJavaScript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/focused.py` | `TypeScriptReader` | 13: `['--term', 'TypeScriptReader', '--term', 'TSXReader',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/snapshot-hashes.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `[{"path":"lizard.py","target":"fc83d74b7d74e6b579d81808451e89661c26d599624c5acc4a41614a46253c71","corrected":"fc83d74b7d74e6b579d81808451e89661c26d599624c5acc4…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/adapted-profile.log` | `lizard_languages/typescript.py`, `typescript.py` | 21: `123500    0.012    0.000    0.108    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/adapted/lizard_languages/typescript.py:74(generate_tokens)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/adapted.diff` | `add_condition` | 60: `self.context.add_condition(pending.defaults)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-1.md.part1` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 63: `` \| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part3` \| `lizard_languages/javascript_bindings.py`, `javascript_bindings.py`, `lizard_langu… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-1.md.part2` | `typescript.py`, `TypeScriptReader` | 12: `` \| `test/test_languages/testTypeScript.py` \| `parameter_count` \| 16: `[(f.name, f.cyclomatic_complexity, f.parameter_count)` \|run: snapshot reader groups, suppl… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/finish-proof.py` | `TypeScriptReader` | 22: `'--term', 'TypeScriptReader', '--term', 'function-results'],` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/literal-git-patches.json` | `add_condition` | 1: `{"raw": {"revisions": ["92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f^", "92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f"], "patch": "diff --git a/lizard_languages/javasc…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/annotations.ts": "93f5227836b388139b742a6ded0234207d1f3020f7bd302a4e107cbf99155386", "affected/arrow-abandoned.js": "56a151a284b6035d556f04483910b8f…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/metadata.json` | `conditions` | 1: `{"baseRefOid":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71","body":"### The problem\n\nThe parameters of an arrow function are counted only when it is assigned to…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/parent-profile.log` | `lizard_languages/typescript.py`, `typescript.py` | 21: `123500    0.012    0.000    0.104    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/parent/lizard_languages/typescript.py:72(generate_tokens)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/raw-profile.log` | `lizard_languages/typescript.py`, `typescript.py` | 21: `123500    0.012    0.000    0.109    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/raw/lizard_languages/typescript.py:72(generate_tokens)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/raw.diff` | `add_condition` | 60: `self.context.add_condition(pending.defaults)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/source-hashes.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/target-profile.log` | `lizard_languages/typescript.py`, `typescript.py` | 21: `123500    0.012    0.000    0.102    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/target/lizard_languages/typescript.py:74(generate_tokens)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/adapted.diff` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 20: `diff --git a/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr544/target/lizard_languages/typescript.py b/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/bootstrap.log` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 6: `{"target": "aa5f40638b23aa54d4724c9429c1ead2183013fd", "head": "3ac7e4eaf61e898836fded88ab715b37f9fedbbf", "baseline": "2e21e404c1ab9f4e0b49acc80816e816629d067…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/bootstrap.py` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 19: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/execution-summary.json` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 17: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/full-pytest.log` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 77: `test/test_languages/testJavaScript.py .........................          [ 42%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/literal-git-patches.json` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 1: `{"raw": {"revisions": ["3ac7e4eaf61e898836fded88ab715b37f9fedbbf^", "3ac7e4eaf61e898836fded88ab715b37f9fedbbf"], "patch": "diff --git a/lizard_languages/javasc…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/make.log` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `conditions`, `TypeScriptReader` | 78: `test/test_languages/testJavaScript.py .........................          [ 42%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/b-short.js": "a73d2a3ef4629b77043d7326ba01058059415c808120452315ecb220749eb07d", "affected/b-story.js": "faf4393a26bc51a330b562c5068695e08cf1fe2b825…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/measure.py` | `lizard_languages/typescript.py`, `typescript.py` | 40: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/metadata.json` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 1: `` {"baseRefOid":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71","body":"### The problem\n\nThe common tokenizer continues a `//` comment on the next line when the lin… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/raw.diff` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 26: `NULLISH_COALESCING, OPTIONAL_ACCESS, TypeScriptReader)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/reproduction.json` | `lizard_languages/typescript.py`, `typescript.py` | 7: `"typescript": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr544/target/lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/snapshot-hashes.json` | `lizard_languages/typescript.py`, `typescript.py` | 15: `"path": "lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/source-hashes.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/adapted.diff` | `lizard_languages/typescript.py`, `typescript.py`, `process_token`, `add_condition`, `conditions`, `TypeScriptReader` | 51: `diff --git a/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr545/target/lizard_languages/typescript.py b/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/bootstrap-summary.json` | `lizard_languages/typescript.py`, `typescript.py` | 7: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/bootstrap.py` | `lizard_languages/typescript.py`, `typescript.py` | 19: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/focused.log` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 13: `test/test_languages/testJavaScript.py .........................          [  4%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/literal-git-patches.json` | `lizard_languages/typescript.py`, `typescript.py`, `process_token`, `add_condition`, `conditions`, `TypeScriptReader` | 1: `{"raw": {"revisions": ["baa95a4df8278421bb3d3a6487516030cc03a979^", "baa95a4df8278421bb3d3a6487516030cc03a979"], "patch": "diff --git a/lizard_languages/javasc…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/abc.js": "56e579ea0676c6bfa16b9004831dd7d75ae14bddd093bd034612ccfedbf3d762", "affected/abc.jsx": "56e579ea0676c6bfa16b9004831dd7d75ae14bddd093bd0346…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/measure.py` | `lizard_languages/typescript.py`, `typescript.py` | 49: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/metadata.json` | `lizard_languages/typescript.py`, `typescript.py`, `conditions` | 1: `{"baseRefOid":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71","body":"### The problem\n\nThe expression of a template interpolation is kept as one token for the\nst…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/raw.diff` | `lizard_languages/typescript.py`, `typescript.py`, `process_token`, `add_condition`, `conditions`, `TypeScriptReader` | 51: `diff --git a/lizard_languages/typescript.py b/lizard_languages/typescript.py` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/reproduction.json` | `lizard_languages/typescript.py`, `typescript.py` | 7: `"typescript": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr545/target/lizard_languages/typescript.py"` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/snapshot-hashes.json` | `lizard_languages/typescript.py`, `typescript.py` | 15: `"path": "lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/source-hashes.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/adapted.diff` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `process_token`, `add_condition`, `conditions`, `TypeScriptReader` | 2: `diff --git a/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr546/target/lizard_languages/typescript.py b/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/bootstrap-summary.json` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 6: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/bootstrap.log` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 11: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/bootstrap.py` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 18: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/literal-git-patches.json` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `process_token`, `add_condition`, `conditions`, `TypeScriptReader` | 1: `{"raw": {"revisions": ["a671632141127fca9b2785fdbb0c675d45ae9042^", "a671632141127fca9b2785fdbb0c675d45ae9042"], "patch": "diff --git a/lizard_languages/typesc…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/manifest.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/b-story.js": "faf4393a26bc51a330b562c5068695e08cf1fe2b82503d1c094ee033c84fa50a", "affected/genuine-all.js": "83ca72e4726df5645621706ea22096693d246da…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/measure.py` | `lizard_languages/typescript.py`, `typescript.py` | 48: `(REPO / 'lizard_languages/typescript.py').read_bytes()` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/measurement-decision.json` | `conditions`, `TypeScriptReader` | 7: `"rationale": "Expanded affected corpus with prior pr544/pr545/pr535/pr536 inputs plus bulk controls and replicas (learning from #544 tiny-corpus inflation and …` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/raw.diff` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `process_token`, `add_condition`, `conditions`, `TypeScriptReader` | 2: `diff --git a/lizard_languages/typescript.py b/lizard_languages/typescript.py` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/reproduction.json` | `lizard_languages/typescript.py`, `typescript.py` | 6: `"typescript": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr546/target/lizard_languages/typescript.py"` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/source-hashes.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `LICENSE.txt` | `conditions` | 17: `the following conditions:` | |
| `lizard_core/file_analyzer.py` | `add_condition`, `conditions` | 66: `conditions = reader.conditions` | |
| `lizard_core/file_info_builder.py` | `add_condition` | 112: `def add_condition(self, inc=1):` | |
| `lizard_ext/lizardcomplextags.py` | `conditions` | 10: `Uses reader.conditions (combined set of all condition types) to track all` | |
| `lizard_ext/lizardcpre.py` | `process_token` | 14: `def preprocess_tokens(tokens):` | |
| `lizard_ext/lizardignoreassert.py` | `add_condition`, `conditions` | 13: `self.conditions = set()` | |
| `lizard_ext/lizardmccabe.py` | `add_condition` | 33: `self.context.add_condition(-1)` | |
| `lizard_ext/lizardmodified.py` | `process_token`, `add_condition`, `conditions` | 18: `reader.context.add_condition()` | |
| `lizard_ext/lizardnd.py` | `conditions` | 68: `# Handle logical operators && and \|\| within conditions` | |
| `lizard_ext/lizardnonstrict.py` | `conditions` | 12: `# Remove logical operators from conditions (non-strict mode)` | |
| `lizard_languages/__init__.py` | `TypeScriptReader` | 21: `from .typescript import TypeScriptReader` | |
| `lizard_languages/clike.py` | `add_condition` | 59: `self.context.add_condition()` | |
| `lizard_languages/code_reader.py` | `process_token`, `conditions` | 105: `def _build_conditions(cls):` | |
| `lizard_languages/erlang.py` | `add_condition` | 123: `self.context.add_condition(curr_cyc_comp)` | |
| `lizard_languages/fortran_states.py` | `add_condition` | 84: `self.context.add_condition()` | |
| `lizard_languages/fortran.py` | `add_condition` | 65: `self.context.add_condition()` | |
| `lizard_languages/javascript_bindings.py` | `add_condition` | 150: `self.context.add_condition(pending.defaults)` | |
| `lizard_languages/javascript.py` | `TypeScriptReader` | 5: `from .typescript import TypeScriptReader` | |
| `lizard_languages/kotlin.py` | `add_condition` | 59: `self.context.add_condition()` | |
| `lizard_languages/perl.py` | `add_condition`, `conditions` | 73: `# Note: _conditions removed - now inherited from PerlReader` | |
| `lizard_languages/php_states.py` | `add_condition` | 65: `self.context.add_condition()` | |
| `lizard_languages/plsql.py` | `add_condition`, `conditions` | 61: `# of keywords to the conditions set for the automatic condition counter` | |
| `lizard_languages/python.py` | `process_token`, `add_condition` | 53: `def process_token(self, token):` | |
| `lizard_languages/r.py` | `conditions` | 168: `# Note: Complexity conditions are automatically counted by the framework` | |
| `lizard_languages/rust.py` | `add_condition` | 50: `self.context.add_condition()` | |
| `lizard_languages/st.py` | `add_condition` | 87: `self.context.add_condition()` | |
| `lizard_languages/swift.py` | `process_token`, `add_condition`, `conditions` | 60: `labels = {k for k in self.conditions if k.isalpha()}` | |
| `lizard_languages/tnsdl.py` | `conditions` | 92: `# Note: No need to define _conditions - base class builds it automatically` | |
| `lizard_languages/tsx.py` | `typescript.py`, `process_token`, `TypeScriptReader` | 4: `Uses TypeScriptStates (from typescript.py) for function detection.` | |
| `lizard_languages/typescript_states.py` | `add_condition` | 218: `self.context.add_condition(summary.defaults)` | |
| `lizard_languages/vue.py` | `TypeScriptReader` | 6: `from .typescript import TypeScriptReader` | |
| `lizard.py` | `conditions` | 11: `#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.` | |
| `theory.rst` | `conditions` | 113: `to check invariants, pre-, post-conditions,` | |

### Test

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_languages/testdata/perl_compound_conditions.pl` | `conditions` | 30: `# Multiple AND conditions` | |
| `test/test_languages/testdata/perl_given_when.pl` | `conditions` | 73: `# Given-when with complex conditions` | |
| `test/test_languages/testdata/perl_nested_subs.pl` | `conditions` | 42: `# Function with nested subroutine that has conditions` | |
| `test/test_languages/testdata/perl_ternary.pl` | `conditions` | 26: `# Ternary with complex conditions` | |
| `test/test_languages/testJavaScriptArrowBoundaries.py` | `conditions` | 144: `def test_continued_body_owns_its_conditions_and_next_statement_does_not(self):` | |
| `test/test_languages/testJavaScriptRegex.py` | `conditions` | 38: `def test_regex_contents_do_not_count_as_conditions(self):` | |
| `test/test_languages/testJSX.py` | `conditions` | 100: `# Multiple conditions` | |
| `test/test_languages/testPerl.py` | `add_condition`, `conditions` | 65: `self.assertEqual(3, method1.cyclomatic_complexity)  # 1 + 2 conditions (if, elsif)` | |
| `test/test_languages/testPython.py` | `conditions` | 176: `self.assertEqual(3, function.cyclomatic_complexity)  # 1 base + 2 conditions (else doesn't count)` | |
| `test/test_languages/testRubyRegex.py` | `conditions` | 25: `def test_split_and_gsub_regex_controls_keep_conditions(self):` | |
| `test/test_languages/testSwiftTokens.py` | `conditions` | 30: `def test_compiler_directive_conditions_are_not_counted(self):` | |
| `test/test_languages/testTypeScript.py` | `TypeScriptReader` | 4: `from lizard_languages import TypeScriptReader` | |
| `test/test_languages/testTypeScriptReaderDispatch.py` | `process_token`, `add_condition`, `TypeScriptReader` | 10: `from lizard_languages.typescript import TypeScriptReader` | |

### Test support or stand-in

none found

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.agents/skills/dough-execute-plan/references/established-session.md` | `conditions` | 107: `with its conditions and stops unchanged. Where that workflow's keep runs` | |
| `.agents/skills/dough-execute-plan/references/execution-decisions.md` | `conditions` | 129: `recurrence, compare current conditions with the recorded cause; reuse evidence` | |
| `.agents/skills/dough-execute-plan/references/one-shot-escalation.md` | `conditions` | 22: `[publication preconditions](trunk-publication.md#preconditions) and authority` | |
| `.agents/skills/dough-execute-plan/references/publish-the-candidate.md` | `conditions` | 24: `## Preconditions` | |
| `.agents/skills/dough-execute-plan/references/trunk-publication.md` | `conditions` | 68: `## Preconditions` | |
| `.agents/skills/dough-execute-plan/references/wrap-up.md` | `conditions` | 46: `assertions/signals at each applicable boundary. Setup supplies preconditions; product and observation establish` | |
| `.agents/skills/dough-execute-plan/SKILL.md` | `conditions` | 74: `[trunk publication's Preconditions](references/trunk-publication.md#preconditions) before` | |
| `.agents/skills/dough-execution-retrospective/references/bounded-process-log.md` | `conditions` | 29: `only when that section's conditions hold. If a safe bounded candidate cannot` | |
| `.agents/skills/dough-post-change-refactor/references/refactor-checks.md` | `conditions` | 80: `Exercise real lower-level production code with crafted preconditions. Do not` | |
| `.agents/skills/dough-story-decomposition/references/problem-decomposition.md` | `conditions` | 79: `stories are cancelled and the safety conditions it must satisfy on its own.` | |
| `.agents/skills/dough-story-decomposition/references/seed-format.md` | `conditions` | 50: `safety conditions this story must satisfy independently` | |
| `.agents/skills/dough-story-refinement/references/executable-proof.md` | `conditions` | 15: `Inspect assertions and setup: distinguish starting preconditions from behavior` | |
| `.agents/skills/dough-story-refinement/references/preparation-journey.md` | `conditions` | 40: `Two conditions end the invocation at the refinement result:` | |
| `.agents/skills/dough-test-optimization/references/optimization-tactics.md` | `conditions` | 36: `- Establish preconditions through direct APIs or testability seams when setup` | |
| `.agents/skills/dough-test-optimization/SKILL.md` | `conditions` | 48: `workers, filters, relevant environment/cache conditions, wall time, executed` | |
| `.agents/skills/dough-triage-project-findings/references/finding-lifecycle.md` | `conditions` | 26: `sufficient for this repair, with which conditions and evidence. Match its effort` | |
| `.agents/skills/dough-triage-project-findings/SKILL.md` | `conditions` | 120: `dates/revisions, evidence locators, material conditions, severity, uncertainty,` | |
| `.claude/skills/dough-execute-plan/references/established-session.md` | `conditions` | 107: `with its conditions and stops unchanged. Where that workflow's keep runs` | |
| `.claude/skills/dough-execute-plan/references/execution-decisions.md` | `conditions` | 129: `recurrence, compare current conditions with the recorded cause; reuse evidence` | |
| `.claude/skills/dough-execute-plan/references/one-shot-escalation.md` | `conditions` | 22: `[publication preconditions](trunk-publication.md#preconditions) and authority` | |
| `.claude/skills/dough-execute-plan/references/publish-the-candidate.md` | `conditions` | 24: `## Preconditions` | |
| `.claude/skills/dough-execute-plan/references/trunk-publication.md` | `conditions` | 68: `## Preconditions` | |
| `.claude/skills/dough-execute-plan/references/wrap-up.md` | `conditions` | 46: `assertions/signals at each applicable boundary. Setup supplies preconditions; product and observation establish` | |
| `.claude/skills/dough-execute-plan/SKILL.md` | `conditions` | 74: `[trunk publication's Preconditions](references/trunk-publication.md#preconditions) before` | |
| `.claude/skills/dough-execution-retrospective/references/bounded-process-log.md` | `conditions` | 29: `only when that section's conditions hold. If a safe bounded candidate cannot` | |
| `.claude/skills/dough-post-change-refactor/references/refactor-checks.md` | `conditions` | 80: `Exercise real lower-level production code with crafted preconditions. Do not` | |
| `.claude/skills/dough-story-decomposition/references/problem-decomposition.md` | `conditions` | 79: `stories are cancelled and the safety conditions it must satisfy on its own.` | |
| `.claude/skills/dough-story-decomposition/references/seed-format.md` | `conditions` | 50: `safety conditions this story must satisfy independently` | |
| `.claude/skills/dough-story-refinement/references/executable-proof.md` | `conditions` | 15: `Inspect assertions and setup: distinguish starting preconditions from behavior` | |
| `.claude/skills/dough-story-refinement/references/preparation-journey.md` | `conditions` | 40: `Two conditions end the invocation at the refinement result:` | |
| `.claude/skills/dough-test-optimization/references/optimization-tactics.md` | `conditions` | 36: `- Establish preconditions through direct APIs or testability seams when setup` | |
| `.claude/skills/dough-test-optimization/SKILL.md` | `conditions` | 48: `workers, filters, relevant environment/cache conditions, wall time, executed` | |
| `.claude/skills/dough-triage-project-findings/references/finding-lifecycle.md` | `conditions` | 26: `sufficient for this repair, with which conditions and evidence. Match its effort` | |
| `.claude/skills/dough-triage-project-findings/SKILL.md` | `conditions` | 120: `dates/revisions, evidence locators, material conditions, severity, uncertainty,` | |
| `.planning/seeds/SEED-010-fabio-malpezzi-pr-reviews.md` | `conditions` | 91: `real arrows still own their defaults and body conditions.` | |
| `.planning/slice-plans/006-structural-js-pr-reviews/SLICE-PROOF.md` | `conditions` | 32: `conditions and methods. Cover arrow and traditional-function IIFEs. Run the` | |
| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` | `lizard_languages/typescript.py`, `typescript.py`, `conditions`, `TypeScriptReader` | 37: `` parsers. `TypeScriptReader` in `lizard_languages/typescript.py` (with `` | |
| `.planning/slice-plans/007-lexical-js-pr-reviews/SLICE-PROOF.md` | `testJavaScript.py`, `conditions` | 22: `` `testJavaScript.py::Test_JavaScript_line_comment_ending_with_a_backslash` `` | |
| `docs/javascript-arrow-boundaries.md` | `TypeScriptReader` | 58: `[testTypeScriptReaderDispatch.py](../test/test_languages/testTypeScriptReaderDispatch.py).` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/review-findings.md` | `conditions` | 29: `return/arrow contexts to retain subsequent conditions and function boundaries.` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md` | `TypeScriptReader` | 112: `\| JavaScriptReader → TypeScriptReader/TypeScriptStates \| run: .js API/CLI reproduction and focused JavaScript/ES6 suites \|` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-supplement.md` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 94: `` \| `test/test_languages/testJavaScript.py` \| `get_js_function_list` \| 7: `def get_js_function_list(source_code):` \| run: complete reader/extension suites; coord… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-1.md` | `lizard_languages/typescript.py`, `typescript.py` | 42: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-2.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `conditions` | 34: `` \| `docs/reviews/fabio-malpezzi-20261027/pr534/manifest.json` \| `lizard_core/function_info.py`, `function_info.py`, `lizard_languages/javascript_bindings.py`, `… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/review-findings.md` | `conditions` | 172: `conditions yielded roughly seven-second warmed batches, above the source's` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/review-findings.md` | `conditions` | 107: `bracket termination, next statements/methods, case/outer conditions and continued` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-1.md` | `lizard_languages/typescript.py`, `typescript.py` | 53: `` \| `docs/reviews/fabio-malpezzi-20261027/pr532/finish-proof.py` \| `lizard_languages/javascript_functions.py`, `javascript_functions.py` \| 35: `'lizard_languages… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-2.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 7: `` \| `TypeScriptReader` \| supplied \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-4.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 8: `` \| `TypeScriptReader` \| supplied \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-2.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 9: `` \| `TypeScriptReader` \| supplied \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace-dispositions.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 14: `--path lizard_languages/typescript.py \` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace-product.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 11: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `TypeScriptReader` | 17: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/README.md` | `TypeScriptReader` | 26: `` PFE: reuse `TypeScriptReader` / `CCppCommentsMixin` / `javascript_templates` `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace-dispositions.md` | `lizard_languages/typescript.py`, `typescript.py`, `process_token`, `conditions`, `TypeScriptReader` | 9: `--term interpolation_code --term process_token --term template_tokens \` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace-product.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `process_token`, `TypeScriptReader` | 9: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `process_token`, `conditions`, `TypeScriptReader` | 41: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/README.md` | `conditions`, `TypeScriptReader` | 1: `# PR545 template-interpolation conditions review` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace.md` | `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `process_token`, `add_condition`, `conditions`, `TypeScriptReader` | 49: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `ongoing/code-structure-reference.md` | `add_condition`, `conditions` | 20: `def _build_conditions(cls):` | |
| `ongoing/condition-categories-reference.md` | `conditions` | 32: `**Purpose**: Operators that combine boolean conditions` | |
| `ongoing/language-implementation-guide.md` | `typescript.py`, `conditions` | 52: `Operators that combine boolean conditions:` | |
| `ongoing/todo_list.md` | `conditions` | 6: `` All phases complete. See `CONDITIONS_REFACTORING.md` for details. `` | |
| `prompt-history.md` | `typescript.py`, `testJavaScript.py`, `TypeScriptReader` | 1: `This project (Lizard) support JSX files, but the implementation is together with javascript support @javascript.py . Please extract the jsx support from the ja…` | |
