# Consumer dispositions for binding correction

Full HEAD trace plus a production-path trace cover the actual effect terms:
`JavaScriptBindings`, `TypeScriptStates`, `expression_arrows`,
`max_nested_structures`, and `analyze_source_code`. The full trace also discovers
incidental matches in changed provenance and evidence filenames. Every located
path from either trace appears below; no row is silently discarded.

Reader import search (`rg -n 'TypeScriptReader|TypeScriptStates|JavaScriptBindings|JavaScriptObjectsMixin' lizard_languages`) confirms the active family:
JavaScriptReader (`js/mjs/cjs`), TypeScriptReader, TSXReader (`tsx/jsx`), VueReader.
No shared CodeStateMachine/CodeReader or other language implementation changed.

`run` records refer to terminal passes in `affected-final.txt` (897 passed, 5
existing skips) and `consumers-final.txt` (499 passed, 2 existing skips).
The complete commands, observer names, setup and expected values are in README.md.
A documentation exclusion creates no new behavior promise. Runtime cost, full
pytest/make, independent refactor, and publication remain coordinator obligations.

| Located consumer | Disposition | Observing proof or exclusion basis |
| --- | --- | --- |
| `.agents/agent-map.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-execute-plan/references/delegation.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-execute-plan/references/finish-or-stop.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-execute-plan/references/story-obligations.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-execute-plan/references/wrap-up.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-execute-plan/scripts/consumer-trace-terms.mjs` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-execute-plan/scripts/dashboard-landing.mjs` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-execute-plan/scripts/story-obligations.mjs` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-land/SKILL.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-product-backlog/references/record-preparation.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/dough-update/SKILL.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/lizard-language-support/SKILL.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/lizard-process-pr/scripts/lizard_measurement.py` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.agents/skills/lizard-process-pr/scripts/pr_packet.py` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-execute-plan/references/delegation.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-execute-plan/references/finish-or-stop.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-execute-plan/references/story-obligations.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-execute-plan/references/wrap-up.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-execute-plan/scripts/consumer-trace-terms.mjs` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-execute-plan/scripts/dashboard-landing.mjs` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-execute-plan/scripts/story-obligations.mjs` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-land/SKILL.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-product-backlog/references/record-preparation.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.claude/skills/dough-update/SKILL.md` | excluded | workflow/skill tooling or guidance, not product parser execution; runtime qualification belongs to coordinator slice 2 |
| `.planning/PRODUCT-BACKLOG.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `.planning/seeds/SEED-023-pr525-binding-boundaries.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `.planning/slice-plans/015-pr525-binding-boundaries/PLAN.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `CHANGELOG.md` | excluded | documentation/example/history; no executable product effect; ADR-0001 metric definitions preserved |
| `DearDough.md` | excluded | documentation/example/history; no executable product effect; ADR-0001 metric definitions preserved |
| `README.rst` | excluded | documentation/example/history; no executable product effect; ADR-0001 metric definitions preserved |
| `docs/adrs/0001-ubiquitous-language-accepted.md` | excluded | documentation/example/history; no executable product effect; ADR-0001 metric definitions preserved |
| `docs/changelog-archive.md` | excluded | documentation/example/history; no executable product effect; ADR-0001 metric definitions preserved |
| `docs/javascript-arrow-boundaries.md` | excluded | documentation/example/history; no executable product effect; ADR-0001 metric definitions preserved |
| `docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/controls/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/paired.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr528/528-profile-baseline.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr528/528-profile-candidate.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr528/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr528/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr528/profile-control.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr528/proposed-patch.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr528/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr528/review-findings.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr528/review-input.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate-isolated.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-import-diagnostic.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-profile-baseline.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-profile-candidate.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/comparison-basis.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/dependency-evidence.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/profile-control.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/proposed-patch.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr531/review-input.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/affected-manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-final-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-supplemental.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/cli-profile.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-supplement.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-1.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-2.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumers-manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-final-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-supplemental.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part1` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part2` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part3` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-proof.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/dependency-535-supplemental.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/final-cli.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/finish-proof.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/full-patch.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/github-comments.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/github-final-state.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/github-patch.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/github-pr.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/github-refresh.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/observe-website.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/prepare-proof.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/profile.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-final-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-focused.part2.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-preexisting.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-red.part2.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-supplemental.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/review-findings.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/run-proof.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/snapshot-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-final-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-preexisting.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-red.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-supplemental.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/validate-current-base.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/website-consumer.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr532/website-integrated.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/authored-tests.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/baseline-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/dependency-evidence.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/independent-child-authored.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/independent-child-expected-token-diagnostic.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/independent-child-patch.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/independent-child-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/profile-control.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/proposed-patch.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-authored.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-expected-token-diagnostic.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-focused.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-profile.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-profile.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/reproduce-harness-api-diagnostic.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/review-findings.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/review-input.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-authored.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-expected-token-diagnostic.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-profile.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-profile.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr534/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/affected-manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-1.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-2.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-3.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-4.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumers-manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-http-after.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-http-before.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part1` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part2` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part3` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-proof.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/finish-proof.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/full.diff` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/github-discussion.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/github-patch.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/github-pr.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/github-threads.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/literal-git-patches.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/run-proof.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/snapshot-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr535/target-regression.log.part1` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/adapted-after-timing.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/adapted-initial-fixture.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/adapted-profile.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/adapted-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/adapted.diff` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/bootstrap.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-1.md.part1` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-1.md.part2` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-2.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/finish-proof.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/literal-git-patches.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/metadata.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/parent-after-timing.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/parent-profile.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/parent-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/profile-invariants.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/profile-proposal.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/raw-after-timing.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/raw-initial-fixture.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/raw-profile.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/raw-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/raw.diff` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/run-proof.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/source-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/target-after-timing.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/target-initial-fixture.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/target-profile.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr536/target-reproduction.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr537/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr537/measurement-decision.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr537/snapshot-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr538/snapshot-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr539/snapshot-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr541/snapshot-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr542/snapshot-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr543/snapshot-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/adapted.diff` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace-dispositions.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace-product.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/literal-git-patches.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/make.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/raw.diff` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr544/source-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/adapted.diff` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace-dispositions.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace-product.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/literal-git-patches.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/raw.diff` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr545/source-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/adapted.diff` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace-dispositions.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace-product.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/literal-git-patches.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/make.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/raw.diff` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr546/source-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr547/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr547/consumer-trace-dispositions.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr547/consumer-trace-product.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr547/consumer-trace.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr547/make.log` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr547/manifest.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr547/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/fabio-malpezzi-20261027/pr547/source-hashes.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/pr525-correction-20261101/evidence/original-packet.json` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/pr525-correction-20261101/evidence/reproduce.py` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/pr525-correction-20261101/implementation/consumer-trace.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/reviews/pr525-issue524-20261101/README.md` | excluded | source/evidence/history, not a live parser consumer; prior review is provenance, not additional promises |
| `docs/website.md` | excluded | documentation/example/history; no executable product effect; ADR-0001 metric definitions preserved |
| `index.py` | run | testJavaScriptBindingBoundaries.py::test_http_api_exposes_complete_callback_and_following_metrics and test/test_website.py; HTTP JSON names/CCN/formal counts/spans |
| `lizard_core/file_analyzer.py` | run | testJavaScriptBindingBoundaries.py plus test/test_analyzer.py; real analyze_source_code dispatch/results |
| `lizard_core/options.py` | run | test/test_options.py and test/testApplication.py; option/threshold/CLI consumers |
| `lizard_ext/lizardcognitive.py` | run | test/test_extensions/testCognitiveComplexity.py; test_javascript and optional-access tests use real reader |
| `lizard_ext/lizardhalstead.py` | run | test/test_extensions/testHalstead.py, testHalsteadClassifier.py, testHalsteadMetrics.py; real extension metrics |
| `lizard_ext/lizardns.py` | run | test_metric_extensions_keep_callback_decisions[NS-*], testJavaScriptNestedStructures.py and generic NS suites; CCN and max_nested_structures assertions |
| `lizard_ext/version.py` | excluded | CHANGELOG mention is guidance comment; release heading/version unchanged |
| `lizard_languages/java_body_states.py` | excluded | Java-only owner; incidental <...> comment match, no affected reader import |
| `lizard_languages/javascript_functions.py` | run | affected-final.txt; signatures/formals/ternary/templates/reader hooks; argument and object cases in testJavaScriptBindingBoundaries.py |
| `lizard_languages/tsx.py` | run | affected-final.txt; signatures/formals/ternary/templates/reader hooks; argument and object cases in testJavaScriptBindingBoundaries.py |
| `lizard_languages/typescript.py` | run | affected-final.txt; signatures/formals/ternary/templates/reader hooks; argument and object cases in testJavaScriptBindingBoundaries.py |
| `lizard_languages/typescript_declarations.py` | run | affected-final.txt; signatures/formals/ternary/templates/reader hooks; argument and object cases in testJavaScriptBindingBoundaries.py |
| `lizard_languages/typescript_signatures.py` | run | affected-final.txt; signatures/formals/ternary/templates/reader hooks; argument and object cases in testJavaScriptBindingBoundaries.py |
| `ongoing/language-implementation-guide.md` | excluded | documentation/example/history; no executable product effect; ADR-0001 metric definitions preserved |
| `prompt-history.md` | excluded | documentation/example/history; no executable product effect; ADR-0001 metric definitions preserved |
| `scripts/website_deployment.py` | excluded | deployment traffic state service-current.json incidental filename match; runtime HTTP adapter covered directly; no deployment action |
| `setup.py` | excluded | reads changelog as package long description; only bug-fix prose added, no packaging/version behavior changed |
| `test/testExtension.py` | run | consumers-final.txt; complete named suite, real analyzer/extension pipeline |
| `test/testHelpers.py` | excluded | ST/C++/Python helper or Python NS observations; affected JS/TS reader not selected |
| `test/testJavaScriptNestedStructures.py` | run | consumers-final.txt; complete named suite, real analyzer/extension pipeline |
| `test/testNestedStructureScopes.py` | run | consumers-final.txt; complete named suite, real analyzer/extension pipeline |
| `test/testNestedStructures.py` | run | consumers-final.txt; complete named suite, real analyzer/extension pipeline |
| `test/testPythonNestedStructures.py` | excluded | ST/C++/Python helper or Python NS observations; affected JS/TS reader not selected |
| `test/test_extensions/testCognitiveComplexity.py` | run | consumers-final.txt; complete named suite, real analyzer/extension pipeline |
| `test/test_extensions/testFanInFanOut.py` | run | consumers-final.txt; complete named suite, real analyzer/extension pipeline |
| `test/test_languages/cobol_helpers.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/dart_helpers.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/java_helpers.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/kotlin_helpers.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/php_helpers.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/swift_helpers.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testArrowObjectBoundaries.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testCAndCPP.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testCsharp.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testES6.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testErlang.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testFortran.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testFortranProcedureBoundaries.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testGDScript.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testGo.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testGoFunctionLiterals.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testJSX.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScript.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScriptArrowBoundaries.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScriptArrowLines.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScriptArrowParameters.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScriptBindingBoundaries.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScriptFormalSlots.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScriptObjectSpread.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScriptParameterNames.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScriptRegex.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testJavaScriptTernaryArrows.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testLua.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testObjC.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testOptionalOperators.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testPLSQL.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testPerl.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testPythonForgive.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testPythonNloc.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testPythonSoftKeywords.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testPythonTypeParameters.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testR.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testRuby.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testRubyControlFlow.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testRubyParser.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testRubyRegex.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testRust.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testRustLetElse.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testScala.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testSolidity.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testTSX.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testTTCN.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_languages/testTypeScript.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testTypeScriptInterpolationConditions.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testTypeScriptReaderDispatch.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testTypeScriptThisParameters.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testVue.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testVueSourceLineNumbers.py` | run | affected-final.txt; complete named suite, public function metrics/boundaries |
| `test/test_languages/testZig.py` | excluded | different language reader/helper; reader-import trace finds JS/TS/Vue/TSX consumers only; no shared base modified |
| `test/test_pr_processing_skill.py` | excluded | PR workflow script contract; no parser implementation or runner changed |
