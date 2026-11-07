---
id: SEED-049
status: active
planted: 2026-11-07
planted_during: lizard process pr
scope: Extension documentation for lizard_ext
---

# SEED-049: Process PR #446

<a id="process-pr-446"></a>

### Process PR #446 — Add documentation for Lizard extensions

**Identity:** SEED-049#process-pr-446
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planless","assessment":"ready","reasons":[],"basis":{"document":"57c1c8b96824e9991b350df701e3e042a0b05badad6f849ac4898332fd1108fb"}}
```

**Goal:** Users discovering Lizard extensions, and maintainers disposing
[PR #446](https://github.com/terryyin/lizard/pull/446), receive one complete
disposition: accurate extension documentation is landed when flags, thresholds,
and examples match current CLI behavior and coexist with the existing
`docs/extensions-and-warnings.rst` pointer, or the PR stays unmerged with an
evidence-backed reason and any required follow-up linked.

**Source:** Dashboard launch instruction
`lizard process pr https://github.com/terryyin/lizard/pull/446`. Full
process-PR scope: review, reproduce claimed commands on current trunk, correct
on the contributor branch when that route exists, qualify, and merge when the
skill's gates hold.

**Beneficiary:** Users who need to discover and use lizard_ext metrics,
analysis, modifiers, and formatters, and maintainers deciding whether to accept
this contribution.

**Scope:** This pull request only. Compare the proposed `lizard_ext/README.md`
and README.rst link against current extension modules and CLI (`python -m
lizard --help`, `-E` loading via `lizard_ext.lizard` + name). Correct a
worthwhile change only on a reachable contributor branch, preserving
contributor history. Reconcile the README.rst insertion with the existing
Extensions and warning controls section so readers are not given two competing
homes. Qualify the final candidate for factual CLI accuracy; full suite and
style checks; performance is unaffected (documentation and images only). Merge
only when those gates and required hosted checks hold; otherwise record the
precise unmet gate.

Deferred promises: rewriting `docs/cli.rst`, redesigning the website, and pull
requests that are not #446. Rejection constraints: do not merge documentation
whose example commands fail on current trunk; do not treat the Fabio cohort's
15-minute deferral or its strict 3% bound as this story's policy.

**Key examples:** Claims to verify on current trunk and on the proposal through
the CLI. Already-covered trunk docs, an unworthy change, or a correction that
cannot be published on the contributor branch receives a recorded reason
instead of a merge.

- Given Nesting Depth docs, examples load the extension (`-END`) before `--ND`
  and show an `ND` column when that path is used.
- Given Nested Structures docs, `-ENS --NS N` works and shows an `NS` column.
- Given duplicated parameter-list docs, the flag matches the module name
  (`-Eduplicated_param_list`), not a collapsed spelling that fails to import.
- Given Cognitive Complexity and Halstead, which trunk already ships, the
  catalog documents `-Ecognitive` / `--CogC` and `-Ehalstead`, or an explicit
  note that they live in `docs/extensions-and-warnings.rst` with a working
  cross-link.
- Given README.rst, readers reach the new catalog without a second competing
  "extensions" section that ignores the existing
  `docs/extensions-and-warnings.rst` pointer.
- Given output-formatter examples (`--csv`, `-H`, `-X`, `--checkstyle`), the
  flags match current `lizard --help`.

Intake metadata: head `dd2f37a09ef5c96e2f1dc7fbcdf13bfdde99e906` on
`Ollec:feature/document_extentions`; PR base
`dd1f9fc51733ded210024a9760d6821997ec97d5`; current master
`caee1cbf243eef4a50c207ceaf9f9634f86e0a18`; `maintainer_can_modify` true;
merge state CLEAN/MERGEABLE; evidence packet under
`.planning/evidence/pr-446/`.
