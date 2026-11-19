# Wait for a long proof run

A running test, build, measurement, or CI run spends no model tokens, and its
time does not count toward the
[slice's active time](oversized-slice.md#track-active-time). Checking on it
does both. Keep the wait in the host's blocking tool until the command
finishes, its deadline procedure applies, or the host hands control back.

- Before launch, set the deadline from the
  [expensive-proof decision](../../dough-story-refinement/references/executable-proof.md#choose-expensive-proof-before-launch)
  or the command's known duration.
- Start the command through the
  [slice clock's `run`](oversized-slice.md#track-active-time), which writes
  the command's output to the log and, when it ends, its exit status to the
  exit file. Keep those outputs and the [run record](proof-evidence.md) in the
  evidence location. Read the exit file for the recorded proof result. Use
  it for the blocking fallback below; a process listing can match the wait
  itself.
- While it runs, make no `sleep`, log, or status calls, and do no other
  checks.
- At the deadline, apply
  [overdue delegated verification](overdue-delegated-verification.md).

## By host

- **Claude Code:** start the command with `run_in_background`, then continue
  independent work or end the turn without a handback. The completion
  notification resumes the agent.
- **Codex:** use the [foreground wait](#codex-foreground-waits).
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

### Codex foreground waits

With `functions.exec` and `functions.wait`, launch the command once inside
one cell. Set the outer launch pragma to
`// @exec: {"yield_time_ms": 300000}` where supported. Use a shorter value if
the host requires it or a caller-side deadline leaves less time. In that
cell, call `tools.exec_command` and wait on any returned shell session with empty
`tools.write_stdin` calls. Set a long inner `yield_time_ms` as well and keep
waiting inside the cell if an inner call returns the session again. Retain
the command's output, exit status, and any workflow receipt.

If the outer call returns a running cell, pass its cell ID to
`functions.wait` with a long `yield_time_ms` (typically `300000` where
supported). Repeat only when the host hands the same live cell back. Use
the longest supported wait within any remaining caller-side deadline. If
the host rejects a duration, use a supported shorter duration on the same
cell or shell session. Retry a launch only when the refusal confirms the
command never started.

Use the original workflow deadline throughout. On a host handback, resume
the pending wait while that deadline still allows it. Apply the workflow's
deadline procedure when its caller-side deadline is reached. When the
command owns the deadline, wait for its terminal receipt and follow the
outcome it reports. An outer yield only hands control back.

With direct shell tools, start with `exec_command` and continue any
returned shell session with empty `write_stdin` calls, using the longest
supported yield within the remaining caller-side deadline. Use the
[blocking exit-file wait](#by-host) when those tools are unavailable.

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
