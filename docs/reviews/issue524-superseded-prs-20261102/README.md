# Open PR review after issue #524 resolution

Current target: `616774e44acc4c4dd277d1cca0577fb54c1feccf`; analyzer and tests
are unchanged from qualified master `99e793fd4144fdf830b9fb917e48e1af091480c5`.
PR #519 head: `3b039e3d1288e225cca3b08f538a3a19e9a6315d`, unchanged on refresh.

## Disposition

No open PR is fully superseded by the #524 fixes, so no PR was closed and no
thank-you or other external comment was posted. The open set remains the same
15 PRs after review. #524 remains closed/completed with its existing resolution
summary; this maintenance request changes none of its disposition.

| Open PR | Screening result |
| --- | --- |
| #306 | Per-line CCN output; unrelated feature |
| #349 | Ruby modified complexity; unrelated behavior |
| #359 | Short/color output; unrelated CLI behavior |
| #369 | C/C++ preprocessing; unrelated reader |
| #390 | Classroom coverage instrumentation and regex flags; unrelated scope, confirmed from its changed files and textual diff |
| #396 | Multi-column CSV extensions; unrelated export change |
| #401 | Fan-in/out metric redesign; unrelated metric |
| #440 | Cognitive complexity metric; distinct feature even where it touches TypeScript |
| #446 | Extension documentation; unrelated documentation |
| #448 | PHP trait function boundaries; unrelated reader |
| #490 | Objective-C interface extension discovery; unrelated reader |
| #506 | HTTP resource limits; distinct endpoint behavior |
| #515 | Documentation/string typos; unrelated cleanup |
| #519 | Related JS/TS boundaries, partly covered; retain open for remaining JSX, location and naming behavior |
| #530 | Python inline suites; unrelated reader |

## PR #519 coverage

The complete five-file diff, issue/inline discussion, one review and three open
threads were read. No existing supersession acknowledgement exists. The check
API returned an empty status list, which is not claimed as successful CI.
There is no merge candidate or runtime change in this maintenance task.

`probe_pr519.py` extracts the exact added classes from the captured head patch,
uses the existing high-level `analyze_source_code` entry point, and checks that
the analyzer was imported from this checkout. Actual discovery finds 16 added
test methods (the proposal body says 18); two existing tokenizer expectations
are also changed in the diff. Against current master: **5 pass, 11 fail**.
Raw assertion differences and the test list are retained in JSON.

Already passing: simple return-type function ends, optional call/index ends,
expression-arrow commas in object properties/arguments, nested object types
in type arguments, and generic object constraints. #525 covers the enclosing
delimiter/binding responsibility; #548 covers the remaining label/annotation
ownership. These do not cover the entire #519 contribution.

Remaining failed assertions include hyphenated, boolean and spread JSX
attributes, member/generic element callback names, JSX comparison containers,
multiline-tag callback locations, parenthesized JSX callback names, an arrow
whose body starts on the next line, and parenthesized function-expression
names. Concrete public failures sufficient to keep the PR open:

- Adding `hidden` to the existing attribute fixture changes component C's
  span from lines 1–7 to 1–10 and puts its successor inside it.
- A callback physically on line 5 of a multiline tag is reported on line 3.
- A comparison in a JSX expression container drops the child callback and
  successor, returning only C at lines 1–8 instead of three functions.

Some naming differences may need a separate product-policy judgment, but the
confirmed span/discovery/location failures already disprove full supersession.
The old branch's three review concerns are retained; this review does not
approve that implementation or reproduce its private-corpus claims. No
broader code correction is authorized or attempted. PR #519 remains the
existing public home for its outstanding contribution.

## Verification

The 490 #524 regression cases still pass on this checkout. Production and
existing test bytes match the qualified merged revision. The first direct
probe invocation could not import the repo module; the corrected probe binds
and verifies its import root before producing the recorded observations.
No performance measurement is required for this metadata-only review.
