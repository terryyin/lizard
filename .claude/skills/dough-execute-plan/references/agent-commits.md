# Agent commits

Use this entry point only when your work has an assigned agent whose
authorship is configured for its workspace: the execution start or preparation
announce result named an `agent`, and its `workspaceAuthorship` is not
`not-configured`. A later session that continues or closes the same work in
that workspace, such as its wrap-up or the landing of its kept preparation,
applies it while that workspace's own Git config still names the agent as
author (`git config --worktree author.name`). Every other commit, such as
caller-selected current-branch work, [one-shot work](one-shot.md), a start
result without an `agent`, or a workspace reported `not-configured`, is made as
before with plain `git commit`.

When it applies, commit your own work in that workspace through the installed
`scripts/agent-commit.mjs` instead of a bare `git commit`. Your Take or
preparation announcement configured you as that workspace's Git author; this
entry point also credits the developer whose Git identity is configured there,
once, as a `Co-authored-by` trailer beside any co-authors your message already
names, such as your host's model trailer.

Stage only the content you own first; the entry point commits what is staged.
Run it from the workspace, or pass `-C <workspace>`, with your message as
`-F <file>`, `-F -` on standard input, or one or more `-m <paragraph>`. Add
`--amend` to amend your own unpublished commit; without a new message it keeps
that commit's message, and a message that already credits the developer keeps
that one credit. It runs an ordinary `git commit`, so the checkout's own
hooks run as usual. Never add `--no-verify`, and do not write the developer
trailer yourself.

Its one-line JSON result on success is `ok: true`, `status` `committed` or
`amended`, `agent`, and the new `sha`. A refusal exits non-zero with
`ok: false`, commits nothing, and leaves staged content staged:

- `developer-identity-refused` — the configured committer in that workspace is
  missing, malformed, or your own agent identity. Report its `error` and stop
  that commit; the developer configures `user.name` and `user.email` there.
  Never commit around it with another identity.
- `no-workspace-agent` — that checkout names no assigned agent as its author,
  although this work expected its agent-configured workspace. You are
  not in your owned workspace; stop and recheck the execution location rather
  than committing elsewhere.
- `commit-failed` — Git or a hook rejected the commit; its `error` carries
  their output. Handle it as that commit's hook or Git failure.

A commit a person makes in the workspace outside your work is theirs; this
entry point is only for commits you make as the agent.
