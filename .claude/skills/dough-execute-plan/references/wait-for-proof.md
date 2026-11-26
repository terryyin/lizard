# Wait for a long proof run

A running test, build, measurement, or CI run spends no model tokens, and its
time does not count toward the
[slice's active time](oversized-slice.md#track-active-time). Checking on it
does both. Start the command once. If the host returns control early,
resume the same wait against the original deadline. Act on the command's
output, exit status, and any workflow receipt as soon as it finishes.

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
- If the caller owns the deadline, bound each wait by the time left and
  follow the workflow's deadline procedure at expiry (for delegated proof,
  [overdue delegated verification](overdue-delegated-verification.md)).
  If the command owns the deadline, await its terminal receipt.

## By host

- **Claude Code:** start the command with `run_in_background`, then continue
  independent work or end the turn without a handback. The completion
  notification resumes the agent.
- **Codex:** use the [foreground wait](#codex-foreground-waits).
- **Cursor:** no notification arrives. Run the command in the foreground when
  it fits the terminal tool's time limit. Otherwise start it in the background
  and wait in one blocking command that ends at the exit file or the deadline;
  repeat that wait only when the tool ends the call first. Set
  `DEADLINE_SECONDS` to the time left before each call:

  ```sh
  end=$(( $(date +%s) + DEADLINE_SECONDS ))
  until [ -f "$EXIT_FILE" ] || [ "$(date +%s)" -ge "$end" ]; do sleep 15; done
  cat "$EXIT_FILE" 2>/dev/null || echo "deadline reached"
  ```

Use the blocking wait on any host whose own facility is unavailable.

### Codex foreground waits

1. Start the `functions.exec` cell with `// @exec: {"yield_time_ms": 300000}`.
   Inside it, launch with `tools.exec_command`. While the result carries a
   shell session ID, await `tools.write_stdin` on that ID with empty input
   inside the cell. Use the longest supported shell yield within any remaining
   caller deadline. Return the accumulated output and final exit status.
2. Whenever Codex returns a running cell ID, resume it with `functions.wait`
   and `yield_time_ms: 300000`. Shorten either outer wait for a nearer
   caller deadline. If a duration is rejected, use a shorter supported wait
   on the same handle. Retry launch only on confirmation it never started.

With direct shell tools, use the same `exec_command` and empty `write_stdin`
loop. If unavailable, use the [blocking exit-file wait](#by-host).

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
`--duration-minutes` when the developer named how long to watch.

In Codex, submit this `functions.exec` call with the supplier command above
filled in:

```javascript
// @exec: {"yield_time_ms": 300000}
let result = await tools.exec_command({
  cmd: "<supplier-watch-command>",
  yield_time_ms: 1000,
});
let output = result.output;
while (result.session_id != null) {
  result = await tools.write_stdin({
    session_id: result.session_id,
    chars: "",
    yield_time_ms: 300000,
  });
  output += result.output;
}
text({ output, exit_code: result.exit_code });
```

Every time the outer call returns a live cell, pass that ID to
`functions.wait`:

```json
{"cell_id": "<live-cell-id>", "yield_time_ms": 300000}
```

Keep the 300,000 ms values from the first call onward. Shorten a wait only
when less time remains before the known absolute deadline or the host refuses
the requested duration. Resume the existing handle when a wait returns early.
Wait on the cell without status checks or progress updates until the command
exits and returns its receipt.

The command prints nothing until it ends itself with one result, which carries
its deadline:

- `publication`: judge the supplier against the exact agreement, as at any
  hold. When the hold lifts, continue the execution. When it stays, keep the
  hold and partial work and start the command again with the same refs and
  `--deadline` set to the returned deadline.
- `deadline`: [stop with the recorded resume condition](execution-decisions.md#stop-or-split-before-more-work).
