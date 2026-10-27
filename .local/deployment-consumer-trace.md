# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `.planning/slice-plans/004-lizard-website-revival/PLAN.md` | changed path |
| `PLAN.md` | basename of `.planning/slice-plans/004-lizard-website-revival/PLAN.md` |
| `docs/website.md` | changed path |
| `website.md` | basename of `docs/website.md` |
| `www.lizard.ws` | supplied |
| `revive-20261027-f818daf` | supplied |
| `roles/storage.objectViewer` | supplied |
| `.planning/slice-plans/004-lizard-website-revival/PLAN` | import path of `.planning/slice-plans/004-lizard-website-revival/PLAN.md` |
| `PLAN` | import path of `.planning/slice-plans/004-lizard-website-revival/PLAN.md`; in 311 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `docs/website` | import path of `docs/website.md` |
| `website` | import path of `docs/website.md` |

## Scope

Whole checkout at `f818daf65e59`, including uncommitted and untracked files: 894 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: `.planning/slice-plans/004-lizard-website-revival/PLAN.md`, `docs/website.md`.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.agents/skills/dough-execute-plan/scripts/story-obligations.mjs` | `PLAN.md` | 195: `"usage: story-obligations.mjs check --plan <PLAN.md> [--slice <N>] [--completion] \| --planned-completion; or list --plan <PLAN.md> --slice <N>",`  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.claude/skills/dough-execute-plan/scripts/story-obligations.mjs` | `PLAN.md` | 195: `"usage: story-obligations.mjs check --plan <PLAN.md> [--slice <N>] [--completion] \| --planned-completion; or list --plan <PLAN.md> --slice <N>",`  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.gcloudignore` | `website` | 13: `!website/`  reused: unchanged upload allowlist inspected; actual deployment contained only approved application files and generated source context |
| `.planning/agents/aino-chan.json` | `website` | 5: `"identity": "SEED-011#revive-lizard-website",`  excluded: unchanged execution identity record; no runtime consumer |
| `app.yaml` | `website` | 12: `static_files: website/static/favicon.ico`  run: actual python313 version deployed; live HTTPS homepage/assets and HTTP adapter observed |
| `index.py` | `website` | 1: `"""The Lizard website: a small HTTP adapter around the public analyzer."""`  run: live preview POST /analyze, malformed JSON, oversized source and valid retry |
| `lizard_core/options.py` | `www.lizard.ws` | 22: `For more information visit http://www.lizard.ws`  excluded: CLI help URL is unchanged; DNS/certificate work does not change CLI behavior |
| `lizard_ext/htmloutput.py` | `www.lizard.ws` | 203: `<a href="http://www.lizard.ws/">Lizard</a> on {{ date }}`  excluded: generated report link is unchanged; no report rendering changes |
| `lizard_ext/lizardmccabe.py` | `website` | 7: `to codebetter's definition available on their website:`  excluded: incidental website word in unchanged source commentary |
| `lizard.py` | `www.lizard.ws`, `website` | 16: `#  Website: www.lizard.ws`  run: real analyzer invoked through deployed shipping_cost HTTP sample |
| `README.rst` | `www.lizard.ws`, `website` | 6: `.. \|Web Site\| image:: http://www.lizard.ws/website/static/img/logo-small.png`  run: both original logo paths returned 200 on the preview; public-host verification remains pending domain ownership |
| `setup.py` | `www.lizard.ws` | 69: `url='http://www.lizard.ws',`  excluded: package metadata is unchanged by operational release record |
| `templates/index.html` | `www.lizard.ws`, `website` | 7: `<link rel="canonical" href="https://www.lizard.ws/">`  run: deployed homepage and all referenced assets returned 200; root owns independent browser journey |

### Test

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_website.py` | `website` | 1: `"""Observe the website through HTTP requests using the real analyzer."""`  reused: accepted 27 HTTP tests on unchanged candidate under Nix Python 3.9 and production Python 3.13.7 |
| `test/testNestedStructures.py` | `website` | 390: `"website": "https://example.com/ada",`  excluded: unrelated analyzer fixture website field; parser unchanged |

### Test support or stand-in

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.planning/seeds/SEED-006-python-version-support.md` | `PLAN.md` | 24: `{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/003-ci-verified-python-support/PLAN.md","assessment":"ready","reasons":[]…`  excluded: unrelated story reference; no change to its plan or behavior |

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.agents/agent-map.md` | `PLAN.md` | 26: `` `slice-plans/<NNN>-<slug>/PLAN.md`, numbered from `001` in allocation order. ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.agents/skills/dough-execute-plan/references/delegation.md` | `PLAN.md` | 11: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' list --plan '<PLAN.md>' --slice N`. ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.agents/skills/dough-execute-plan/references/finish-or-stop.md` | `PLAN.md` | 195: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --completion`. ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.agents/skills/dough-execute-plan/references/story-obligations.md` | `PLAN.md` | 106: `` `<PLAN.md>` to the active plan, and `N` to the current slice: ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.agents/skills/dough-execute-plan/references/wrap-up.md` | `PLAN.md` | 186: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --slice N`. ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.agents/skills/dough-land/SKILL.md` | `PLAN.md` | 93: `node <installed>/dough-execute-plan/scripts/story-obligations.mjs check --plan <PLAN.md> --planned-completion`  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.agents/skills/dough-product-backlog/references/record-preparation.md` | `PLAN.md` | 235: `` `slice-plans/075-correction/PLAN.md`). For `--approach planned`, `--plan` is ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.claude/skills/dough-execute-plan/references/delegation.md` | `PLAN.md` | 11: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' list --plan '<PLAN.md>' --slice N`. ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.claude/skills/dough-execute-plan/references/finish-or-stop.md` | `PLAN.md` | 195: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --completion`. ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.claude/skills/dough-execute-plan/references/story-obligations.md` | `PLAN.md` | 106: `` `<PLAN.md>` to the active plan, and `N` to the current slice: ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.claude/skills/dough-execute-plan/references/wrap-up.md` | `PLAN.md` | 186: `` `node '<installed-execute-plan>/scripts/story-obligations.mjs' check --plan '<PLAN.md>' --slice N`. ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.claude/skills/dough-land/SKILL.md` | `PLAN.md` | 93: `node <installed>/dough-execute-plan/scripts/story-obligations.mjs check --plan <PLAN.md> --planned-completion`  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.claude/skills/dough-product-backlog/references/record-preparation.md` | `PLAN.md` | 235: `` `slice-plans/075-correction/PLAN.md`). For `--approach planned`, `--plan` is ``  excluded: generic lifecycle guidance/tool references PLAN.md; no change to plan schema or tooling contract |
| `.planning/PRODUCT-BACKLOG.md` | `PLAN.md`, `website` | 9: `- [Claim support only for the Python versions CI verifies](seeds/SEED-006-python-version-support.md#ci-verified-python-support) — SEED-006#ci-verified-python-s…`  excluded: unchanged backlog link; parent owns execution lifecycle |
| `.planning/seeds/SEED-010-fabio-malpezzi-pr-reviews.md` | `PLAN.md` | 18: `{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/004-fabio-malpezzi-pr-reviews/PLAN.md","assessment":"ready","reasons":[],…`  excluded: unrelated story reference; no change to its plan or behavior |
| `.planning/seeds/SEED-011-lizard-website-revival.md` | `PLAN.md`, `website` | 10: `# SEED-011: Lizard website revival`  reused: release scope and public-host ownership gate inspected; parent owns planning disposition |
