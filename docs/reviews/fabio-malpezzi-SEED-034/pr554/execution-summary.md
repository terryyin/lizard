# PR #554 execution summary

**Disposition:** left unmerged with follow-up SEED-035#ns-member-name-performance.
**Reason:** quiet-load cumulative `api-parameters` under `--extension ns` paired
95% upper bound exceeds the strict &lt;3% gate (attempt5:
median +3.638%, CI [+2.770%, +4.513%]). Incremental gates all pass; cumulative
api passes. Semantic candidate adequate as-authored; no ≤15-minute product fix
clears the cumulative budget already spent by the accepted set measured without
`-ENS`.

**Candidate head:** `2368413b2de05b05ff44ce863dcded8061342492`
**PR remains OPEN** — no merge, no force-close.

**Evidence:** `README.md`, `reproduction.json`, `attempt5/`, prior attempts.
**Follow-up:** `.planning/seeds/SEED-035-ns-member-name-performance.md`
**Backlog:** first of Backlog list after SEED-034 completion / during disposition.
