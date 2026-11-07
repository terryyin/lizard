# Post-change refactor judgment — PR #553

Independent review after applying the PR as-authored on current master.

The change is a single token-pattern widening in `OPTIONAL_NAME` plus one
parametrized regression. No parallel parser, no PR-specific switch, no
duplicate concept. Existing optional-access / nullish / ternary tokenization
remains the owner. No structural refactor warranted within the correction
budget; leave as-authored.
