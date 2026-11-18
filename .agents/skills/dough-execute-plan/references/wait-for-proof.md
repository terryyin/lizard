# Wait for a long proof run

A running test, build, measurement, or CI run spends no model tokens, and its
time does not count toward the
[slice's active time](oversized-slice.md#track-active-time). Checking on it
does both. Wait so that no model turn runs until the command finishes or
reaches its deadline.

- Before launch, set the deadline from the
  [expensive-proof decision](../../dough-story-refinement/references/executable-proof.md#choose-expensive-proof-before-launch)
  or the command's known duration.
- Start the command through the
  [slice clock's `run`](oversized-slice.md#track-active-time), which writes
  the command's output to the log and, when it ends, its exit status to the
  exit file. Keep those outputs and the [run record](proof-evidence.md) in the
  evidence location. Wait on that exit file, never on a process listing: `pgrep -f`
  can match the wait itself.
- While it runs, make no `sleep`, log, or status calls, and do no other
  checks.
- At the deadline, apply
  [overdue delegated verification](overdue-delegated-verification.md).

## By host

- **Claude Code:** start the command with `run_in_background`, then continue
  independent work or end the turn without a handback. The completion
  notification resumes the agent.
- **Codex:** start the command with `exec_command`. While it runs, continue the
  session with `write_stdin`, sending no input and using the longest
  `yield_time_ms` the tool accepts.
- **Cursor:** no notification arrives. Run the command in the foreground when
  it fits the terminal tool's time limit. Otherwise start it in the background
  and wait in one blocking command that ends at the exit file or the deadline;
  repeat that wait only when the tool ends the call first:

  ```sh
  end=$(( $(date +%s) + DEADLINE_SECONDS ))
  until [ -f "$EXIT_FILE" ] || [ "$(date +%s)" -ge "$end" ]; do sleep 15; done
  cat "$EXIT_FILE" 2>/dev/null || echo "deadline reached"
  ```

Use the blocking wait on any host whose own facility is unavailable.

## Watch a paused supplier

Start the installed [publication wait](../scripts/wait-for-publication.mjs)
once from the execution checkout and await it as a long proof run on this host:

```sh
node '<installed-execute-plan>/scripts/wait-for-publication.mjs' --remote '<authorized remote>' \
  --ref '<target branch>:<supplier story home>' \
  --ref '<target branch>:<home recording this dependency agreement>' \
  --ref '<supplier branch>'
```

Name the supplier's branch only when the remote holds one, and add
`--duration-minutes` when the developer named how long to watch. The command
prints nothing until it ends itself with one result, which carries its
deadline:

- `publication`: judge the supplier against the exact agreement, as at any
  hold. When the hold lifts, continue the execution. When it stays, keep the
  hold and partial work and start the command again with the same refs and
  `--deadline` set to the returned deadline.
- `deadline`: [stop with the recorded resume condition](execution-decisions.md#stop-or-split-before-more-work).
