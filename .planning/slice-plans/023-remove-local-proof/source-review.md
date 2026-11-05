# Source review — SEED-036#remove-local-proof

Launch: fresh general-purpose reviewer with source-only inputs (Goal, Scope,
key examples, exclusions). No plan, product, or proof. Agent ID
`c0c5fa77-0736-46ae-8626-3cc20506ee40`.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | A Lizard developer who maintains the website and its tests works in a smaller repository that no longer carries a local Cucumber proof collection or persistence feature. | slice 1 | observed: observed: website/frontend/proof gone; recorder modules deleted; Intent lacks proof fields; test:e2e:dry exit 0 without plugin |
| 2 | Ordinary local tests and GitHub Actions CI proof continue to serve their existing purposes. | slice 1 | observed: observed: test:e2e:dry exit 0; workflow keeps CI proof fragment/summarize --expect test=1 and website/frontend/features=1; scripts/ci-proof untouched |
| 3.1 | Delete the local website Cucumber proof feature: the retained snapshot tree | slice 1 | observed: observed: git rm website/frontend/proof/latest.json; path absent |
| 3.2 | Delete: the recorder/plugin and refresh implementation | slice 1 | observed: observed: git rm proof-plugin.js proof-snapshot.js |
| 3.3 | Delete: the deliberate recording path | slice 1 | observed: observed: cucumber.mjs has no RECORD_CUCUMBER_PROOF plugin; package.json has no recording script |
| 3.4 | Delete: dedicated proof tests and their exclusive helpers/dependencies | slice 1 | observed: observed: deleted proof-plugin.test.js proof-refresh.test.js cucumber-runner.js; proper-lockfile uninstalled |
| 3.5 | Delete: local `proof` / `proofGeneration` settings | slice 1 | observed: observed: .planning/open-dough.json gherkin location has no proof/proofGeneration |
| 3.6 | Delete: project-owned documentation or agent guidance that describes that feature | slice 1 | observed: observed: docs/website.md recording/proof narrative removed; managed Open Dough guidance left untouched |
| 4 | Remove `website/frontend/proof` and every local-proof-only historical snapshot, fixture record or copied artifact. | slice 1 | observed: observed: website/frontend/proof tree deleted; no replacement archive path |
| 5 | Keep no archive or replacement history store. | slice 1 | observed: observed: no archive or history-store path added; proof tree deleted outright |
| 6.1 | Remove `website/frontend/proof-plugin.js` | slice 1 | observed: observed: website/frontend/proof-plugin.js deleted |
| 6.2 | Remove `website/frontend/proof-snapshot.js` | slice 1 | observed: observed: website/frontend/proof-snapshot.js deleted |
| 6.3 | Remove the `RECORD_CUCUMBER_PROOF` collection path | slice 1 | observed: observed: no RECORD_CUCUMBER_PROOF in product tree (rg); cucumber.mjs has no env gate |
| 6.4 | Remove Cucumber plugin wiring | slice 1 | observed: observed: cucumber.mjs export has no plugin field |
| 6.5 | Remove recording commands | slice 1 | observed: observed: open-dough.json proofGeneration command removed; docs recording command removed |
| 6.6 | Remove the dedicated `test:proof` script | slice 1 | observed: observed: package.json scripts lack test:proof; workflow Cucumber proof recorder step removed |
| 7 | Remove local `proof` and `proofGeneration` settings from `.planning/open-dough.json`. | slice 1 | observed: observed: .planning/open-dough.json intent locations have no proof or proofGeneration keys |
| 8 | Preserve the Intent source location and CI proof entry identities (`test` and `website/frontend/features`). | slice 1 | observed: observed: open-dough.json keeps path test and website/frontend/features; workflow --entry/--expect for both remain |
| 9 | Remove the dedicated proof-plugin and proof-refresh tests. | slice 1 | observed: observed: proof-plugin.test.js and proof-refresh.test.js deleted |
| 10 | Remove every helper, fixture, mock, dependency or package entry used only by the feature (including exclusive support such as `proper-lockfile` when nothing else needs it). | slice 1 | observed: observed: cucumber-runner.js deleted; proper-lockfile removed from package.json and package-lock.json |
| 11 | Trace exclusive dependencies until no orphaned support remains. | slice 1 | observed: observed: rg proper-lockfile under website/frontend finds none |
| 12 | Remove local-proof documentation, comments and project-owned agent guidance. | slice 1 | observed: observed: docs/website.md local-proof sections removed; project-owned agent guidance had none beyond that |
| 13.1 | Delete the feature with no disabled implementation | slice 1 | observed: observed: no stub/disabled recorder files remain under website/frontend |
| 13.2 | Delete the feature with no compatibility reader | slice 1 | observed: observed: no compatibility reader for proof snapshots remains |
| 13.3 | Delete the feature with no opt-in switch | slice 1 | observed: observed: RECORD_CUCUMBER_PROOF opt-in removed from cucumber.mjs and docs |
| 13.4 | Delete the feature with no stub | slice 1 | observed: observed: no stub proof plugin or empty proof directory left |
| 13.5 | Delete the feature with no historical feature narrative | slice 1 | observed: observed: docs/website.md no longer narrates the recording feature |
| 13.6 | Delete the feature with no migration guide | slice 1 | observed: observed: no migration guide added; feature deleted without replacement guidance |
| 14 | Spent story/plan records for this work are cleaned at closure, not as a substitute for deleting the product feature. | landing |  |
| 15.1 | Preserve analyzer behavior | slice 1 | observed: observed: nix develop -c python -m pytest test/test_analyzer.py - 22 passed |
| 15.2 | Preserve website behavior | slice 1 | observed: observed: cucumber.mjs paths/import/format preserved; test:e2e:dry 16 scenarios |
| 15.3 | Preserve ordinary local test execution (including `test:e2e` pass/fail semantics, reports and diagnostics) | slice 1 | observed: observed: npm --prefix website/frontend run test:e2e:dry exit 0; reports format still html:reports/cucumber.html |
| 15.4 | Preserve GitHub Actions' native test reports | slice 1 | observed: observed: workflow Browser tests and Keep browser test reports steps remain |
| 15.5 | Preserve CI proof fragments | slice 1 | observed: observed: workflow Write the CI proof fragments steps remain for test and website/frontend/features |
| 15.6 | Preserve summary artifacts | slice 1 | observed: observed: workflow summarize path with --expect entries remains |
| 15.7 | Preserve the dashboard contract established by Publish Lizard CI proof for Intent / the CI proof publisher under `scripts/ci-proof/` | slice 1 | observed: observed: scripts/ci-proof/ unchanged; no local recorder refs; fragment/summarize expects preserved |
| 16 | Shared code remains only where a surviving test or CI responsibility needs it. | slice 1 | observed: observed: only exclusive proof helpers/deps removed; cucumber/playwright/vite remain |
| 17 | Delete local-proof collection and persistence entry points. | slice 1 | observed: observed: product-tree search finds no proof-plugin/RECORD_CUCUMBER/test:proof/proofGeneration entry points |
| 18 | Receive shared Open Dough guidance removal through Open Dough's released update rather than editing its installed managed copies. | slice 1 | observed: observed: .agents/.claude dough-test-optimization proof references unchanged (git status) |
| 19 | Open Dough's own story owns published guidance and payload cleanup. | slice 1 | observed: observed: no edits to installed managed Open Dough guidance copies |
| 20 | Deferred: richer CI proof than today's contract. |  | G1 |
| 21 | Deferred: any replacement local verification archive. |  | G2 |
| 22 | Deferred: changing Intent entry identities beyond dropping local-proof fields. |  | G3 |
| 23 | Assumption: CI proof (report → fragment → summary → dashboard) is a separate surviving responsibility from the deleted local recorder. | slice 1 | observed: observed: scripts/ci-proof and workflow CI proof path retained separately from deleted recorder |
| 24 | Assumption: ordinary review evidence and CI run artifacts keep their actual surviving purpose and need no local fallback. | slice 1 | observed: observed: docs/reviews and CI artifact upload steps retained; no local fallback added |
| 25.1 | Journey: those paths, scripts and settings are gone | slice 1 | observed: observed: proof tree, modules, RECORD wiring, test:proof script/settings gone |
| 25.2 | Journey: ordinary `test:e2e` still runs without a proof plugin | slice 1 | observed: observed: cucumber.mjs has no plugin; test:e2e:dry succeeds |
| 25.3 | Journey: `.planning/open-dough.json` still names the Gherkin Intent location without `proof` or `proofGeneration` | slice 1 | observed: observed: open-dough.json gherkin location without proof/proofGeneration |
| 26.1 | Journey: workflow still produces fragments and a summary for those same entry identities | slice 1 | observed: observed: workflow fragment --entry and summarize --expect for test and website/frontend/features remain |
| 26.2 | Journey: the Cucumber proof-recorder job step that only ran `test:proof` is gone with the feature | slice 1 | observed: observed: Cucumber proof recorder tests step removed from python-app.yml |
| 27.1 | Journey: local proof collection/persistence references, exclusive fixtures and orphaned dependencies are absent | slice 1 | observed: observed: one-time rg audit: no product local-proof collection/persistence entry points or proper-lockfile |
| 27.2 | Journey: analyzer tests, website tests and `scripts/ci-proof/` remain | slice 1 | observed: observed: analyzer pytest pass; website features and scripts/ci-proof remain |
| 27.3 | Journey: dedicated feature tests are deleted rather than replaced by a permanent absence checker | slice 1 | observed: observed: dedicated proof tests deleted; no absence-checker test added |
| 27.4 | Journey: a one-time whole-product reading/search is the deletion audit | slice 1 | observed: observed: deletion audit was one-time search during slice, not a permanent checker |
| 28 | Do not leave a disabled or stubbed local-proof path. | slice 1 | observed: observed: no disabled/stubbed local-proof path in website/frontend |
| 29 | Do not invent a permanent absence checker in place of deleting dedicated feature tests. | slice 1 | observed: observed: no permanent absence checker; dedicated tests deleted |
| 30 | Do not weaken or replace CI proof publication. | slice 1 | observed: observed: CI proof fragment/summarize/expect path preserved |
| 31 | Do not edit installed managed Open Dough guidance in this story. | slice 1 | observed: observed: installed managed Open Dough guidance not edited |
