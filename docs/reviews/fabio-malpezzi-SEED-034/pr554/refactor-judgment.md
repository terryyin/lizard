# Post-change refactor judgment — PR #554

Independent review after applying the PR as-authored on current master.

Adds a small state in the existing NS state machine so `.` / `?.` skip a
following structure keyword. No parallel counter, no PR-specific switch.
No structural refactor warranted; leave as-authored.
