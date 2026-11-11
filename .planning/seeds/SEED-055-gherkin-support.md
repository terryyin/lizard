---
id: SEED-055
status: active
planted: 2026-11-11
scope: story
---

# Report line counts for Gherkin feature files

<a id="gherkin-support"></a>

### Report line counts for Gherkin feature files

**Identity:** SEED-055#gherkin-support
```json dough-story-state
{"schemaVersion":1,"refinement":"refined","approach":"planned","plan":"../slice-plans/033-gherkin-line-counts/PLAN.md","assessment":"ready","reasons":[],"basis":{"document":"eff689b0a17da47448771b1ce93939581fb00e2cb7f47b524e89dbca4030f77d","plan":"2310fa390fa19992677c249ba9ae972888bf76c0625cc535b170c6712346c867"}}
```

**Goal.** Teams that maintain Gherkin specifications can use Lizard to see how
long each written scenario and shared setup block is in their `.feature`
files, using the existing API, reports, and NLOC warning thresholds.

**Scope.**

- Discover `.feature` files through the normal language-reader registry,
  including directory analysis and language selection with `-l gherkin`.
- Report one Lizard function for each written `Background`, `Scenario`, and
  `Scenario Outline`, including the English aliases `Example` and
  `Scenario Template`. Recognize blocks inside `Rule` as well as directly
  inside `Feature`. Neither container is a function.
- Count nonblank, non-comment source lines in each block, including its
  declaration, description, steps, data tables, doc-string delimiters and
  nonblank content, and attached `Examples`/`Scenarios` sections and rows.
  Comments are lines whose first non-whitespace character is `#` outside a
  doc string. A `#` in step text, a table cell, or a doc string is data.
- A block ends before the next block or `Feature`/`Rule` container, or at
  EOF. Report its declaration line and last counted content line. A new
  container's description belongs to that container, not the previous block.
  Tags before a scenario declaration are outside that function and must not
  leak into its predecessor. Tags decorating an outline's Examples section
  remain part of the outline's contents.
- Preserve the written block kind and title in its function name, including
  an unnamed `Background`. Distinct declarations with the same title remain
  distinct results at their own source locations.
- Every reported function has CCN `1`: prose, table values, and example-row
  counts do not introduce control flow. Background lines count only in their
  own function; outline rows never expand into additional function results.

**Deferred promises.** Localized keywords, executing specifications, expanding
outlines, resolving step definitions, syntax validation and recovery
diagnostics, and a new Gherkin-specific complexity metric. English is the
working boundary from the seed's keywords. This boundary adds no rejection rule
for naturally handled input. Existing file-read errors retain normal Lizard
handling. Token-count and parameter semantics receive no new metric contract.

**Required restrictions.** Do not interpret text inside doc strings or data
tables as block declarations or comments: doing so would fabricate results or
lose source lines. CCN stays `1` and shared setup is not copied into scenarios
because the goal measures written source, not execution paths or runtime cost.

**Key examples.**

1. A `.feature` file contains a Feature header, a Background header and one
   step, a Scenario header and two steps, and an Outline header, two steps,
   an Examples header, a table header and two data rows. Analyzing the file
   produces three functions with NLOC `2`, `3`, and `7`, each CCN `1`.
   Blank separators do not change those counts. Directory CSV analysis and
   `-l gherkin` expose the same three functions.
2. A block has an ordinary description, a blank line, a full-line comment,
   and a step whose text contains `#`, `if`, or `&&`. Analysis counts the
   description and step, excludes the blank/comment lines, and retains CCN
   `1`. A tag before the following Scenario does not enlarge this block.
3. A step has a doc string containing `Scenario: quoted data` and
   `# literal data`, with a blank content line. Analysis keeps one function,
   counts the nonblank content and both delimiters, and invents no scenario.
   Triple quotes and triple backticks have the same ownership rule; data
   table cells containing header-looking text are also data.
4. A Rule follows a Scenario, then contains an unnamed Background and an
   `Example`. Analysis ends the first Scenario before the Rule, omits the
   Rule's header/description from all three block counts, and reports the
   Background and Example separately. The last block is reported even without
   a trailing newline. A file with no block declaration has no functions.
5. An Outline has two Examples sections, one tagged, and several rows.
   Analysis still reports one outline whose NLOC includes those sections and
   their data. Reusing a title elsewhere never merges the declarations.

**Alternatives and recommendation.**

| Candidate | Fit to the same goal | Consequence |
| --- | --- | --- |
| Existing raw/file line count or a manual text script | Smallest workaround, but supplies neither block ownership nor Lizard function warnings | Does not deliver the selected outcome |
| A Gherkin language reader using existing analysis/reporting | Preserves written blocks as functions and existing NLOC/CCN/report contracts | Recommended; add only Gherkin lexical ownership |
| A full Gherkin AST parser | Can supply syntax validation and dialect coverage, but those exceed the current promise | Larger dependency and semantic surface; defer unless English extraction proves insufficient |

**Borrowed mechanism.** Direct-cost accounting assigns each charge once to
its owning cost centre. Here, a written block owns its source lines; a
Background is a separate shared-setup account and an Outline owns its table
once. This prevents allocation by number of consumers or execution rows. The
analogy stops at runtime effort: NLOC does not estimate test duration, weighted
complexity, or maintainability by itself.

**Challenge and failure boundaries.** The strongest consequential doubt is
whether users could read these results as execution complexity: an outline
with many rows and a repeatedly used Background can meet that interpretation
very differently. The existing seed explicitly chooses written blocks and
CCN `1`; keep that meaning visible in maintained documentation. An unavailable
step-definition dependency cannot prevent source analysis because no execution
or resolution is needed. An unterminated doc string can hide later apparent
headers; diagnostic recovery is deferred, and the user repairs the delimiter
and reruns analysis. Do not promise validated counts for malformed Gherkin.

**Architecture.** Reuse `CodeReader`, the registered-language discovery path,
`FileInfoBuilder`, and the shared counters/reporters. Gherkin owns recognition
of declarations, containers, tags and opaque literal content. A read-only
review and disposable local probe found no need for a new result model,
reporter, core counter, runtime parser dependency, or cross-cutting boundary.
[ADR-0001: Ubiquitous language](../../docs/adrs/0001-ubiquitous-language-accepted.md)
governs Function, Language reader, NLOC and CCN;
[ADR-0000: Use ADRs](../../docs/adrs/0000-use-adrs-accepted.md) preserves human
ownership of architectural decisions. Both are current Accepted records;
there is no conflict or exception in this direction.

**Evidence.** At preparation base
`9db4c39d51e17d7e2732f159175c277e7cf2e92a`, `.feature` has no registered
reader; the API returns zero functions and directory/explicit-file CSV has
no rows for example 1. A disposable reader probe through the real API and
CLI produced the expected `2/3/7` counts, CCN `1`, and source locations;
doc-string/container/EOF ownership also passed. Its proof failed without
the reader, and all scratch edits were reverted before this seed write.
The [official Gherkin reference](https://cucumber.io/docs/gherkin/reference/)
(consulted 2026-11-11) establishes aliases, containers, Examples and literal
syntax. Precise tagged-boundary handling remains implementation work, not
an observed completed capability. No unobservable premise changes the
selected written-source goal.
