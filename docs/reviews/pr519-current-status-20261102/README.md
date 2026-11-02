# Remaining TypeScript and JSX boundary cases

[PR #519](https://github.com/terryyin/lizard/pull/519) remains open. Its
TypeScript/TSX function-boundary contribution is only partly covered by the
merged [#525](https://github.com/terryyin/lizard/pull/525) and
[#548](https://github.com/terryyin/lizard/pull/548) fixes and earlier changes.
The reported [#524](https://github.com/terryyin/lizard/issues/524) crash and
its related delimiter/label/annotation cases remain resolved.

Against master `616774e` (same analyzer as `99e793f`), the 16 added test
methods at PR head `3b039e3` produce 5 passes and 11 assertion failures.
These include distinct remaining JSX behavior:

- Boolean, hyphenated and spread attributes can extend a component into its
  successor. In the proposal's attribute fixture, C ends at line 10 instead
  of line 7 after adding `hidden`.
- Callbacks in multiline tag attributes can be reported before their source
  line: a callback at line 5 is reported at line 3.
- A comparison in a JSX expression container can hide the child callback and
  following component, leaving only the outer component at lines 1–8.

Further failed assertions concern callback/function names in member or
generic JSX tags and parenthesized expressions, and an arrow whose expression
body starts on the next line. Naming differences need their own compatibility
judgment. The span, location and discovery failures already establish that
the whole PR is not superseded. Its original implementation also has three
open review threads and has not been approved by this comparison.

All 490 label/annotation regression cases and the full 2917-test suite pass
with 7 existing skips. No product change or new performance claim accompanies
this review. The other 14 open PRs concern independent functionality; none
was closed as a consequence of #524's resolution.
