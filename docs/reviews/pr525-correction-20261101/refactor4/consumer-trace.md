# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md` | changed path |
| `PLAN.md` | basename of `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md` |
| `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md` | changed path |
| `source-review.md` | basename of `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md` |
| `DearDough.md` | changed path |
| `docs/reviews/pr525-correction-20261101/README.md` | changed path |
| `README.md` | basename of `docs/reviews/pr525-correction-20261101/README.md` |
| `DD-398` | supplied |
| `issuecomment-6561098652` | supplied |
| `b97f7958bd63bdafce692cf926c70d927410717b` | supplied |
| `pr525-correction-20261101` | supplied |

## Scope

Whole checkout at `e68c735e1156`, including uncommitted and untracked files: 1983 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `DearDough.md`, `docs/reviews/pr525-correction-20261101/README.md`.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.agents/skills/dough-execute-plan/scripts/story-obligation-source-review-record.mjs` | `source-review.md` | 136: `"Link the source review with one “**Source review:** [text](source-review.md)” line.",` | |
| `.agents/skills/dough-execute-plan/scripts/story-obligation-source-review.mjs` | `source-review.md` | 102: `` // unlinked `source-review.md` beside the plan, is refused rather than read as `` | |
| `.agents/skills/dough-execute-plan/scripts/story-obligations-cli.mjs` | `PLAN.md` | 5: `'usage: story-obligations.mjs check --plan <PLAN.md> [--slice <N>] [--completion] \| --planned-completion; or list --plan <PLAN.md> --slice <N>; or record --pla…` | |
| `.claude/skills/dough-execute-plan/scripts/story-obligation-source-review-record.mjs` | `source-review.md` | 136: `"Link the source review with one “**Source review:** [text](source-review.md)” line.",` | |
| `.claude/skills/dough-execute-plan/scripts/story-obligation-source-review.mjs` | `source-review.md` | 102: `` // unlinked `source-review.md` beside the plan, is refused rather than read as `` | |
| `.claude/skills/dough-execute-plan/scripts/story-obligations-cli.mjs` | `PLAN.md` | 5: `'usage: story-obligations.mjs check --plan <PLAN.md> [--slice <N>] [--completion] \| --planned-completion; or list --plan <PLAN.md> --slice <N>; or record --pla…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-1.md.part2` | `PLAN.md`, `README.md` | 37: `` \| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/PLAN.md` \| `.parameters` \| 201: `Reported: slice 6 — "The corrected PR532 changes shared FunctionInfo.pa… `` | |
| `docs/reviews/pr525-correction-20261101/evidence/parent-safe-preflight.json` | `pr525-correction-20261101` | 6: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/evidence/parent-safe-preflight-spec.json"` | |
| `docs/reviews/pr525-correction-20261101/evidence/verify_final.py` | `pr525-correction-20261101` | 14: `default=Path('docs/reviews/pr525-correction-20261101/verification'))` | |
| `docs/reviews/pr525-correction-20261101/hosting/delivery-receipt.json` | `issuecomment-6561098652`, `b97f7958bd63bdafce692cf926c70d927410717b` | 5: `"merge_commit": "b97f7958bd63bdafce692cf926c70d927410717b",` | |
| `docs/reviews/pr525-correction-20261101/hosting/final-packet/terryyin--lizard--pr525/c4d66fec350682d032d9656410fa920cb5d65fa2ff5c54dbdb04bf3dfb7cccfa/diff.patch` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `DearDough.md`, `docs/reviews/pr525-correction-20261101/README.md`, `README.md`, `pr525-correction-20261101` | 1: `diff --git a/.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md b/.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md` | |
| `docs/reviews/pr525-correction-20261101/hosting/final-packet/terryyin--lizard--pr525/c4d66fec350682d032d9656410fa920cb5d65fa2ff5c54dbdb04bf3dfb7cccfa/packet.json` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `DearDough.md`, `docs/reviews/pr525-correction-20261101/README.md`, `README.md`, `pr525-correction-20261101` | 3688: `"blob_url": "https://github.com/terryyin/lizard/blob/e68c735e11564e3afb7b543f8b3ad7cc1a06b3a0/.planning%2Fslice-plans%2F015-pr525-binding-boundaries%2FPLAN.md",` | |
| `docs/reviews/pr525-correction-20261101/hosting/merge-guard.json` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `docs/reviews/pr525-correction-20261101/README.md`, `README.md`, `pr525-correction-20261101` | 284: `".planning/slice-plans/015-pr525-binding-boundaries/PLAN.md",` | |
| `docs/reviews/pr525-correction-20261101/hosting/merged-master-ci.json` | `b97f7958bd63bdafce692cf926c70d927410717b` | 2: `"sha": "b97f7958bd63bdafce692cf926c70d927410717b",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt1/arguments-api.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt1/arguments-api.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt1/current-api.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt1/current-api.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt1/current-ns.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt1/current-ns.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt1/summary.json` | `pr525-correction-20261101` | 16: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt1/current-api.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt2/arguments-api.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt2/arguments-api.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt2/arguments-ns.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt2/arguments-ns.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt2/current-api.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt2/current-api.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt2/current-cli.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt2/current-cli.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt2/current-ns.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt2/current-ns.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt2/parent-api.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt2/parent-api.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt2/parent-cli.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt2/parent-cli.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt2/parent-ns.json` | `pr525-correction-20261101` | 13: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt2/parent-ns.json",` | |
| `docs/reviews/pr525-correction-20261101/measurements/attempt2/summary.json` | `pr525-correction-20261101` | 16: `"/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries/docs/reviews/pr525-correction-20261101/measurements/attempt2/current-api.json",` | |

### Test

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_pr_processing_skill.py` | `PLAN.md` | 187: `(tmp_path / "PLAN.md").write_text("accepted proof\n")` | |

### Test support or stand-in

none found

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.agents/agent-map.md` | `PLAN.md` | 26: `` `slice-plans/<NNN>-<slug>/PLAN.md`, numbered from `001` in allocation order. `` | |
| `.agents/skills/dough-execute-plan/references/delegation.md` | `PLAN.md` | 11: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' list --plan '<PLAN.md>' --slice N`. `` | |
| `.agents/skills/dough-execute-plan/references/finish-or-stop.md` | `PLAN.md`, `DearDough.md` | 194: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --completion` `` | |
| `.agents/skills/dough-execute-plan/references/story-obligations.md` | `PLAN.md`, `source-review.md` | 110: `` the plan as `source-review.md`. When you retain it, link it from the plan's `` | |
| `.agents/skills/dough-execute-plan/references/wrap-up.md` | `PLAN.md`, `source-review.md` | 26: `` retain them beside the plan as `source-review.md` in its `` | |
| `.agents/skills/dough-execution-retrospective/references/process-finding-recording.md` | `DearDough.md` | 8: `` Use the explicit user/project `DearDough.md` location or `<project-root>/DearDough.md` for enabled `` | |
| `.agents/skills/dough-execution-retrospective/SKILL.md` | `DearDough.md` | 10: `` May plan corrections, record process findings in `DearDough.md` (500-line warning, 1,000-line `` | |
| `.agents/skills/dough-land/SKILL.md` | `PLAN.md`, `DearDough.md` | 17: `` records supported findings in `DearDough.md`. `` | |
| `.agents/skills/dough-maintain-findings/SKILL.md` | `DearDough.md` | 5: `project's DearDough.md while preserving evidence and former-code traceability.` | |
| `.agents/skills/dough-product-backlog/references/record-preparation.md` | `PLAN.md` | 235: `` `slice-plans/075-correction/PLAN.md`). For `--approach planned`, `--plan` is `` | |
| `.agents/skills/dough-story-wrap-up/SKILL.md` | `DearDough.md` | 11: `` and records supported findings in `DearDough.md`. `` | |
| `.agents/skills/dough-triage-project-findings/SKILL.md` | `DearDough.md` | 38: `` - The canonical `DearDough.md` location: the project's explicit location takes `` | |
| `.agents/skills/lizard-process-pr/references/experience.md` | `DearDough.md` | 47: `\| An example is fixed but nearby grammar forms, later functions or public metrics regress. \| [#517 review](https://github.com/terryyin/lizard/pull/517#issuecom…` | |
| `.claude/skills/dough-execute-plan/references/delegation.md` | `PLAN.md` | 11: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' list --plan '<PLAN.md>' --slice N`. `` | |
| `.claude/skills/dough-execute-plan/references/finish-or-stop.md` | `PLAN.md`, `DearDough.md` | 194: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --completion` `` | |
| `.claude/skills/dough-execute-plan/references/story-obligations.md` | `PLAN.md`, `source-review.md` | 110: `` the plan as `source-review.md`. When you retain it, link it from the plan's `` | |
| `.claude/skills/dough-execute-plan/references/wrap-up.md` | `PLAN.md`, `source-review.md` | 26: `` retain them beside the plan as `source-review.md` in its `` | |
| `.claude/skills/dough-execution-retrospective/references/process-finding-recording.md` | `DearDough.md` | 8: `` Use the explicit user/project `DearDough.md` location or `<project-root>/DearDough.md` for enabled `` | |
| `.claude/skills/dough-execution-retrospective/SKILL.md` | `DearDough.md` | 10: `` May plan corrections, record process findings in `DearDough.md` (500-line warning, 1,000-line `` | |
| `.claude/skills/dough-land/SKILL.md` | `PLAN.md`, `DearDough.md` | 17: `` records supported findings in `DearDough.md`. `` | |
| `.claude/skills/dough-maintain-findings/SKILL.md` | `DearDough.md` | 5: `project's DearDough.md while preserving evidence and former-code traceability.` | |
| `.claude/skills/dough-product-backlog/references/record-preparation.md` | `PLAN.md` | 235: `` `slice-plans/075-correction/PLAN.md`). For `--approach planned`, `--plan` is `` | |
| `.claude/skills/dough-story-wrap-up/SKILL.md` | `DearDough.md` | 11: `` and records supported findings in `DearDough.md`. `` | |
| `.claude/skills/dough-triage-project-findings/SKILL.md` | `DearDough.md` | 38: `` - The canonical `DearDough.md` location: the project's explicit location takes `` | |
| `.planning/PRODUCT-BACKLOG.md` | `PLAN.md` | 9: `- [Correct, qualify and merge PR #525](seeds/SEED-023-pr525-binding-boundaries.md#correct-pr525-binding-boundaries) — SEED-023#correct-pr525-binding-boundaries…` | |
| `.planning/seeds/SEED-023-pr525-binding-boundaries.md` | `PLAN.md`, `README.md` | 9: `{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/015-pr525-binding-boundaries/PLAN.md","assessment":"ready","reasons":[],"…` | |
| `AGENTS.md` | `README.md` | 52: `` - Human propose / discuss / approve process: `docs/adrs/README.md` `` | |
| `docs/adrs/0000-use-adrs-accepted.md` | `README.md` | 22: `` `docs/adrs/README.md`. `` | |
| `docs/adrs/0001-ubiquitous-language-accepted.md` | `README.md` | 64: `- Links: playbook [README.md](./README.md); ADR-0000` | |
| `docs/adrs/README.md` | `README.md` | 72: `` \| `docs/adrs/README.md` \| This playbook \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md` | `README.md` | 143: `64 related affected inputs, all frozen before timing. README.md gives replay.` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-supplement.md` | `README.md` | 133: `` \| `docs/reviews/fabio-malpezzi-20261027/pr533/README.md` \| `paired.py` \| 27: `manifest.json beside the external corpus and identical paired.py. The original` \|… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-2.md` | `PLAN.md`, `README.md` | 153: `` \| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/PLAN.md` \| `534-review.md`, `CONTEXT.md`, `parameters`, `534-review` \| 22: `- [Design constraints, prepa… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/review-findings.md` | `README.md` | 85: `README.md. These commands run from the prepared execution checkout; expand R` | |
| `docs/reviews/fabio-malpezzi-20261027/pr534/README.md` | `PLAN.md`, `DearDough.md` | 16: `` `.planning/slice-plans/011-ternary-arrow-boundaries/PLAN.md`; `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-2.md` | `PLAN.md` | 159: `` \| `.planning/slice-plans/004-lizard-website-revival/PLAN.md` \| `parameter_count` \| 66: `` `nix develop -c python -c 'from lizard import analyze_file; result=an… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-3.md` | `PLAN.md` | 80: `` \| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/PLAN.md` \| `.parameters` \| 198: `Reported: slice 6 — "The corrected PR532 changes shared FunctionInfo.pa… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-4.md` | `PLAN.md` | 94: `` \| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/PLAN.md` \| `parameter_count`, `TypeScriptReader`, `.parameters`, `full_parameters` \| 24: `- JS inherits … `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-2.md` | `PLAN.md` | 92: `` \| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/PLAN.md` \| `parameter_count`, `.parameters`, `TypeScriptReader` \| 24: `- JS inherits TypeScriptReader. T… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace-dispositions.md` | `PLAN.md` | 61: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| excluded: planning authority \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace-product.md` | `PLAN.md` | 122: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`,… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace.md` | `PLAN.md`, `README.md` | 184: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`,… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace-dispositions.md` | `PLAN.md` | 62: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| excluded: planning authority \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace-product.md` | `PLAN.md`, `README.md` | 137: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/typescri… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace.md` | `PLAN.md`, `DearDough.md`, `README.md` | 433: `` \| `.planning/slice-plans/006-structural-js-pr-reviews/PLAN.md` \| `matrix` \| 54: `` \\| Required hosted checks remain the python-app matrix builds plus `changes`… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace-dispositions.md` | `PLAN.md` | 58: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| excluded: planning authority \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace-product.md` | `PLAN.md`, `README.md` | 229: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| `lizard_languages/typescript.py`, `typescript.py`, `conditions`, `TypeScriptReader` \| 37: `` pars… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace.md` | `PLAN.md`, `DearDough.md`, `README.md` | 399: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| `lizard_languages/typescript.py`, `typescript.py`, `conditions`, `TypeScriptReader` \| 37: `` pars… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/consumer-trace-dispositions.md` | `PLAN.md` | 58: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| excluded: planning authority \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/consumer-trace-product.md` | `PLAN.md`, `README.md` | 210: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `testOpti… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/consumer-trace.md` | `PLAN.md`, `README.md` | 373: `` \| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` \| `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `testOpti… `` | |
| `docs/reviews/fabio-malpezzi-20261027/story-map.md` | `README.md` | 11: `` \|[SEED-010#review-structural-js-prs](pr537/README.md)\|537–543\|Done; dispositions in `pr537`–`pr543` (recovery `e480e5d`); #540 already-fixed/closed.\| `` | |
| `docs/reviews/pr525-correction-20261101/cost-return/consumer-dispositions.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `README.md`, `pr525-correction-20261101` | 8: `` \| `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md` \| excluded \| Retained evidence or workflow/domain documentation; incidental term/path match, no … `` | |
| `docs/reviews/pr525-correction-20261101/cost-return/consumer-trace-product.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `README.md`, `pr525-correction-20261101` | 205: `` \| `docs/reviews/pr525-correction-20261101/cost-return/current-arguments-profile.json` \| `lizard_languages/javascript_bindings.py`, `javascript_bindings.py`, `l… `` | |
| `docs/reviews/pr525-correction-20261101/cost-return/README.md` | `pr525-correction-20261101` | 39: `nix develop -c python docs/reviews/pr525-correction-20261101/evidence/profile_runtime.py --checkout /Users/terryyin/.cache/pr525-correction-qualified-20261101/…` | |
| `docs/reviews/pr525-correction-20261101/cost/consumer-dispositions.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `DearDough.md`, `README.md`, `pr525-correction-20261101` | 30: `` \| `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md` \| excluded \| Retained evidence or workflow/domain documentation; incidental term/path match, no … `` | |
| `docs/reviews/pr525-correction-20261101/cost/consumer-trace-product.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `DearDough.md`, `README.md`, `pr525-correction-20261101` | 338: `` \| `docs/reviews/pr525-correction-20261101/cost/current-profile.json` \| `lizard_languages/javascript_bindings.py`, `javascript_bindings.py`, `lizard_languages/j… `` | |
| `docs/reviews/pr525-correction-20261101/cost/consumer-trace.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `DearDough.md`, `README.md`, `pr525-correction-20261101` | 7: `` \| `docs/reviews/pr525-correction-20261101/cost/consumer-trace.md` \| changed path \| `` | |
| `docs/reviews/pr525-correction-20261101/cost/README.md` | `pr525-correction-20261101` | 30: `nix develop -c python docs/reviews/pr525-correction-20261101/evidence/prepare_measurements.py --cache /Users/terryyin/.cache/pr525-correction-qualified-20261101` | |
| `docs/reviews/pr525-correction-20261101/implementation/consumer-dispositions.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `DearDough.md`, `README.md`, `pr525-correction-20261101` | 15: `The complete commands, observer names, setup and expected values are in README.md.` | |
| `docs/reviews/pr525-correction-20261101/implementation/consumer-trace-product.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `README.md`, `pr525-correction-20261101` | 184: `` \| `docs/reviews/pr525-correction-20261101/evidence/original-packet.json` \| `lizard_languages/javascript_bindings.py`, `javascript_bindings.py`, `lizard_languag… `` | |
| `docs/reviews/pr525-correction-20261101/implementation/consumer-trace.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `DearDough.md`, `README.md`, `pr525-correction-20261101` | 7: `` \| `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md` \| changed path \| `` | |
| `docs/reviews/pr525-correction-20261101/implementation/README.md` | `PLAN.md`, `pr525-correction-20261101` | 13: `The coordinator's PLAN.md/evidence edits are separate ownership.` | |
| `docs/reviews/pr525-correction-20261101/refactor1-consumer-trace.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `README.md`, `pr525-correction-20261101` | 184: `` \| `docs/reviews/pr525-correction-20261101/evidence/original-packet.json` \| `lizard_languages/javascript_bindings.py`, `javascript_bindings.py`, `lizard_languag… `` | |
| `docs/reviews/pr525-correction-20261101/refactor2/consumer-dispositions.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `DearDough.md`, `README.md`, `pr525-correction-20261101` | 14: `` \| `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md` \| excluded: Retained evidence or workflow/domain documentation; incidental term/path match, no r… `` | |
| `docs/reviews/pr525-correction-20261101/refactor2/consumer-trace-product.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `DearDough.md`, `README.md`, `pr525-correction-20261101` | 338: `` \| `docs/reviews/pr525-correction-20261101/cost/candidate-hashes.json` \| `lizard_languages/javascript_bindings.py`, `javascript_bindings.py`, `lizard_languages/… `` | |
| `docs/reviews/pr525-correction-20261101/refactor3/consumer-dispositions.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `README.md`, `pr525-correction-20261101` | 7: `` \| `.agents/skills/lizard-process-pr/scripts/lizard_measurement.py` \| reused \| Public parameter consumption is observed on unchanged bytes by cost-return/post-e… `` | |
| `docs/reviews/pr525-correction-20261101/refactor3/consumer-trace-product.md` | `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md`, `PLAN.md`, `.planning/slice-plans/015-pr525-binding-boundaries/source-review.md`, `source-review.md`, `README.md`, `pr525-correction-20261101` | 205: `` \| `docs/reviews/pr525-correction-20261101/cost-return/candidate-hashes.json` \| `lizard_languages/javascript_bindings.py`, `javascript_bindings.py`, `lizard_lan… `` | |
