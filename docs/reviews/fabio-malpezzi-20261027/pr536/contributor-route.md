# Contributor correction route, SEED-016 slice 1

Observed 2026-10-27 from the execution checkout on
`codex/report-consistent-javascript-and-typescript-arro`, HEAD
`7f479101728b5fec8becaf7c784b752ed736719e`.

The live PR remains [#536](https://github.com/terryyin/lizard/pull/536), open
and unmerged, with the intended unchanged contribution:

- Head: `FabioMalpezzi/lizard:pr/js-arrow-parameters`,
  `92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f`.
- PR base reference: `master`; the PR API reports base SHA
  `01afe9c228ba2a3dc5253259d8415d2ef47f0b71`.
- Actual current target from `git ls-remote origin refs/heads/master
  refs/pull/536/head`: `master` is
  `7f479101728b5fec8becaf7c784b752ed736719e`; PR head is the SHA above.
- Fork branch from `git ls-remote git@github.com:FabioMalpezzi/lizard.git
  refs/heads/pr/js-arrow-parameters`: the same head SHA.
- `maintainerCanModify: true`, observed with
  `nix develop -c gh pr view 536 --repo terryyin/lizard --json
  state,headRefName,headRefOid,headRepositoryOwner,headRepository,baseRefName,baseRefOid,maintainerCanModify,mergeable,url`.

The connected GitHub identity is `terryyin`. The connector's complete PR diff
contains the same three-file proposal retained in `raw.diff`: a second
`_read_name` collector in `javascript_bindings.py`, pending return-type binding
handling and bare-arrow capture, the binding-candidate change in
`typescript_states.py`, and three regression tests appended to `testES6.py`.
The body contains no prerequisite PR claim. Fresh connector calls returned no
issue or inline comments, review submissions, or review threads.

The connector returned one PR-triggered `Python application` run for this head:
[run 40418663046](https://github.com/terryyin/lizard/actions/runs/40418663046),
completed successfully (workflow ID 38306292). Commit status contexts were
empty. These observations concern the original head; they do not qualify a
corrected revision or replace its required final CI.

The collaborator-permission endpoint was unavailable through the GitHub
integration: HTTP 403, `Resource not accessible by integration`. This limits
that API observation, rather than establishing denied Git publication.

An authenticated, non-mutating Git receive-pack preview succeeded:

```sh
git push --dry-run --porcelain git@github.com:FabioMalpezzi/lizard.git 92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f:refs/heads/pr/js-arrow-parameters
```

```text
To github.com:FabioMalpezzi/lizard.git
=	92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f:refs/heads/pr/js-arrow-parameters	[up to date]
Done
```

Exit status was 0. Together with `maintainerCanModify: true`, this establishes
a reachable authenticated contributor-head route for implementation to
continue. The no-op preview creates no accepted publication and does not test
server-side checks for a changed candidate. Slice 5 must refresh the head,
preserve its history, obtain real non-force push acceptance for the qualified
correction, and verify exact-head CI/merge/acknowledgement. No actual push,
merge, comment, branch switch, commit, or product change occurred in this
slice.
