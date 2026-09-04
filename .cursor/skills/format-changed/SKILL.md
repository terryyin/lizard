---
name: format-changed
description: >-
  On-demand preparation of working-tree changes for commit by selectively
  checking style on affected components. Use when explicitly requested outside
  the routine execute-plan wrap-up; stop when a style result needs semantic or
  design judgment.
---

# Format Changed

Prepare the current working-tree changes for commit without taking ownership of
Git state. The repository command owns changed-component detection; do not
reproduce its path-to-component mapping in this skill or in ad hoc shell logic.

This is an explicitly on-demand skill. Routine `execute-plan` wrap-up runs the
same repository command directly and does not invoke this skill or spawn a
formatting agent.

## Workflow

1. From the repository root, record `git status --short` so the final report can
   identify the changed paths present before formatting.
2. Run the selective style command through the repository runner:

   ```bash
   ./scripts/run.sh make format-changed
   ```

   The command considers staged, unstaged, and nonignored untracked paths and
   checks only their affected pep8 components. It reports style issues; it does
   not rewrite files.
3. If the command reports a mechanically determined correction, make only that
   mechanical correction and rerun the command. Examples include
   formatter-prescribed whitespace, import ordering, or another result with one
   unambiguous non-behavioral fix.
4. If resolving a result could change behavior, public types, architecture, or
   design intent, stop without guessing. Preserve any formatting already
   applied and report the diagnostic and the judgment needed.
5. Record `git status --short` again and report every changed path remaining,
   including paths added or modified during formatting.

Never stage, commit, push, revert, restore, stash, or hide changes. Do not run
`make pep8`, `make pylint`, or duplicate component detection. Do not broaden the
task beyond mechanical pep8 results.

## Handoff

On success, end the response with:

```markdown
## FORMAT CHANGED COMPLETE
```

Include the command result and changed-path list immediately before the marker.

When judgment is required, include the failing diagnostic, changed-path list,
and the decision needed, then end with:

```markdown
## FORMAT CHANGED JIDOKA STOP
```
