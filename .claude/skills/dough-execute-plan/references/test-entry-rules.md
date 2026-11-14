# Select shared conventions before test work

Use the project established by the task. When choosing or changing a test's
scope, apply [the test boundary rule](../../dough-post-change-refactor/references/refactor-checks.md#choose-the-test-boundary).
Before authoring or changing tests, identify their project-relative source
paths, including intended paths for new tests. Run the shared selector from
this installation:

```sh
node '<installed-execute-plan>/scripts/test-entry-rules.mjs' \
  --project '<project-root>' --source '<test-source>'
```

Repeat `--source` for additional paths. Resolve `<installed-execute-plan>` to the
directory containing this skill, independently of the target project. The command
reads that project's `.planning/open-dough.json` and applies the same
`intent.locations` declarations and file membership as Intent. Folder containment
alone does not establish membership. It accepts intended paths before files exist.

For each source, load only its selected conventions. `pageObjects: true` loads
[the page-object convention](../../dough-page-objects/references/convention.md)
when the task concerns browser tests. `makeMe: true` independently loads
[the MakeMe convention](../../dough-makeme/references/convention.md).
If a selected convention's guidance is unavailable, report that rule as
unresolved; do not replace it with
an invented convention or silently treat it as disabled. Leave changes requiring
that rule unperformed and continue independent work. The command's `status`
reports applicability, not whether a convention was available or applied.

A rule is enabled only by the exact boolean `true` on at least one matching
entry. A matching opt-out never vetoes another entry's opt-in. Omitted, false
and non-boolean flags are off. Neither rule implies the other. Missing
configuration means off; malformed or unreadable configuration returns
`status: "unresolved"` and a nonzero command result. Report that problem and stop
changes that depend on the unresolved selection. Do not label it successful opt-out.

For planned and planless execution, run this check before test-authoring
delegation. Carry source paths, selected conventions and unresolved checks in
the brief. For new tests or changed boundaries, include the intended layer,
entry point and any necessary service substitute. If exact new test paths
emerge during implementation, require the author to check them before
authoring. Recheck when paths or scope change.
Selection adds no authority to convert unrelated tests and imposes no browser
abstraction on non-browser work.

## Select implicated tests for refactoring

During post-change refactoring, run the same selector with changed-source scope:

```sh
node '<installed-execute-plan>/scripts/test-entry-rules.mjs' \
  --project '<project-root>' --changed
```

When the caller supplies committed dispositions since the last refactor pass,
append `--committed '<revision>'` for each supplied commit, or an explicit Git
range for supplied range dispositions. A single revision means that commit
only; a range uses Git's diff comparison. Add `--path '<source>'` for explicit
caller-supplied paths or newly implicated existing sources. Read those diffs too;
do not add arbitrary committed history. The shared Git discovery includes staged
and unstaged changes independently, untracked non-ignored files, and both sides
of moves. Selection then uses existing source files, following source symlinks,
at their current paths and the same entry membership above. Missing or deleted
sources and missing symlink targets require no conversion;
a destination outside selected membership receives no page-object rule.
Production-only changes select no testing convention merely by folder proximity.

Load each selected convention once, including the same references above.
The page-object convention applies to implicated browser tests. Correct enabled
tests through existing focused owners while preserving scenario inputs, expectations and independent
evidence. Leave other entries' practices intact. Missing selected guidance and
unresolved configuration keep the same stop conditions above. Recheck selection
if refactoring implicates additional paths; selection never expands authority.
