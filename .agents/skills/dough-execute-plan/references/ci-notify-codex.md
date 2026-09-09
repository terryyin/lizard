# Codex CI notification adapter

Follow [ci-monitor.md](ci-monitor.md) for CI selection and failure recovery.

With `functions.exec`, `yield_control`, `notify`, `tools.exec_command`, and
`tools.write_stdin`, start one yielded observer cell when execution begins,
before the first push. On reentry, use the active plan's observer note to reuse
a running cell and terminal `finished` entries to avoid restarting completed observation. Recover
that note before considering replacement when volatile handles are lost.
Substitute verified repository, checkout, and coordinator below:

```js
const key = 'ci-watch-execution:OWNER/REPO:BRANCH:COORDINATOR'
if (load(key)?.status === 'finished') exit()
try {
  let result = await tools.exec_command({
    cmd: 'node /ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs stream --execution OWNER/REPO BRANCH',
    workdir: '/ABSOLUTE/VERIFIED/CHECKOUT_ROOT',
    tty: true,
    yield_time_ms: 1000,
    max_output_tokens: 2000,
  })
  let tail = ''
  let directory
  let pid
  let terminal
  const events = []
  const consume = (chunk) => {
    const lines = `${tail}${chunk}`.split('\n')
    tail = lines.pop()
    for (const line of lines) {
      if (line.startsWith('CI_OBSERVER_RESULT ')) {
        terminal = JSON.parse(line.slice('CI_OBSERVER_RESULT '.length)).terminal
        continue
      }
      if (line.startsWith('CI_OBSERVER ')) {
        ;({ directory, pid } = JSON.parse(line.slice('CI_OBSERVER '.length)))
        continue
      }
      if (!line.startsWith('{')) continue
      const record = JSON.parse(line)
      if (record.event?.type?.startsWith('CI_')) events.push(record.event)
    }
  }
  const deliver = () => {
    for (const event of events.splice(0)) notify(event)
  }
  consume(result.output)
  text({
    key,
    status: result.session_id ? 'watching' : 'finished',
    sessionId: result.session_id,
    directory,
    pid,
    tail,
    terminal,
  })
  await yield_control()
  deliver()
  while (result.session_id) {
    result = await tools.write_stdin({
      session_id: result.session_id,
      chars: '',
      yield_time_ms: 1000,
      max_output_tokens: 2000,
    })
    consume(result.output)
    deliver()
  }
  store(key, {
    status: terminal?.status === 'stopped' ? 'stopped' : 'finished',
    sessionId: undefined,
    directory,
    pid,
    tail,
    terminal,
  })
} catch (error) {
  store(key, { status: 'lost' })
  notify({ type: 'CI_MONITOR_UNAVAILABLE', key, reason: String(error).slice(-1000) })
}
```

The initial yielded output exposes the session, directory, and PID. Save them
with the cell ID, coordinator, and checkout in the active plan before the first
push. Treat that note as the live handle: writes to `store` in a running cell
may remain invisible to other cells until it finishes. Do not use cross-cell
`load`/`store` mutations to coordinate shutdown or detect a running observer.
The parser retains chunk tails, consumes initial output before yielding, then notifies
queued events. Subsequent reads notify immediately; awaited work keeps the cell
alive. Continue delegation after yielding. `notify` delivers at the coordinator's
next boundary without model polling. Do not repeatedly `wait`, assign a watching
agent, or broaden ordinary network/filesystem permissions.

Without the yielded-cell tools, report monitoring unavailable once and continue;
never substitute recurring AI polling or claim notifications from a background
shell or file alone. Use another native bridge only when its delivery contract
has been independently supplied and verified for the installed host.

When the shared [observer lifecycle](ci-monitor.md#own-one-observer) calls
for shutdown:

- With the receipt directory from the plan note, run
  `node /ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs stop DIRECTORY`
  from the verified checkout. Let the existing reader consume the stream's
  terminal result, then reap its cell with one bounded wait. Do not issue a
  second `write_stdin` while that reader owns the PTY: concurrent reads can
  consume each other's terminal output and invalidate the process handle.
  Confirm the stop receipt, terminal result, and process exit before marking
  the plan note stopped. Cell termination alone proves no subprocess exit.
- Without handles, recover the plan note. Match coordinator/checkout and validate
  the saved directory's `request.json` root, repository, branch, and execution
  mode. Run `node /ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs stop DIRECTORY`
  from that checkout. Read its terminal receipt and `result.json`; confirm the
  recorded PID disappears using a finite local `ps` wait. Never signal that PID.
  Missing/mismatched identity means no guessed stop, newest-mailbox lookup, or
  replacement launch. Older unidentified observers cannot be recovered. Stop
  errors, missing terminal evidence, or unconfirmed exit mean unresolved
  shutdown; never force termination or claim closure.

Preserve recorded failures and report unread events and `pendingCi: unobserved`,
not green CI. Consume delivered failures before completion; never wait for pending
CI. Keep acknowledgment and repair semantics unchanged. On resume, rearm only
absent, `stopped`, or `lost` observers after confirming the old process ended;
never restart terminal `finished` observers.

Normal and repair pushes retain the key, session, directory, process, and cell.
The observer discovers successive selected-branch pushes, persists events before streaming,
and owns observation until shutdown. Changed HEAD/SHA never requires setup again.
