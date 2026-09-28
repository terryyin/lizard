# Codex CI notification adapter

Follow [ci-monitor.md](ci-monitor.md) for CI selection and failure recovery.

With `functions.exec`, `yield_control`, `notify`, `tools.exec_command`, and
`tools.write_stdin`, start one yielded observer before the first publication it
must cover, using the authorized target as `BRANCH`. Reuse the
observer note in the active plan (planned) or conversation (quick) and terminal
`finished` entries; recover that note before replacement when handles are lost.
Resolve `/ABSOLUTE/RESOLVED/SKILL` inside `/ABSOLUTE/VERIFIED/CHECKOUT_ROOT` with
[runtime setup](runtime-setup.md). Do not arm when setup stops.

This isolate cannot import Node modules. Do not reconstruct parsing or extra
readers. Copy this host binding with verified repository, checkout, and
coordinator; do not add parser branches.

```js
const key = 'ci-watch-execution:OWNER/REPO:BRANCH:COORDINATOR'
if (load(key)?.status === 'finished') exit()
const io = { yield_time_ms: 1000, max_output_tokens: 2000 }
let tail = '', directory, pid, terminal
const events = []
const consume = (chunk) => {
  const lines = `${tail}${chunk}`.split('\n')
  tail = lines.pop()
  for (const line of lines) {
    if (line.startsWith('CI_OBSERVER_RESULT ')) terminal = JSON.parse(line.slice('CI_OBSERVER_RESULT '.length)).terminal
    else if (line.startsWith('CI_OBSERVER ')) ({ directory, pid } = JSON.parse(line.slice('CI_OBSERVER '.length)))
    else if (line.startsWith('{')) {
      const event = JSON.parse(line).event
      if (event?.type?.startsWith('CI_')) events.push(event)
    }
  }
}
const deliver = () => { for (const event of events.splice(0)) notify(event) }
try {
  let result = await tools.exec_command({
    cmd: 'node /ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs stream --execution OWNER/REPO BRANCH',
    workdir: '/ABSOLUTE/VERIFIED/CHECKOUT_ROOT',
    tty: true,
    ...io,
  })
  consume(result.output)
  text({ key, status: result.session_id ? 'watching' : 'finished', sessionId: result.session_id, directory, pid, tail, terminal })
  await yield_control()
  deliver()
  while (result.session_id) {
    result = await tools.write_stdin({ session_id: result.session_id, chars: '', ...io })
    consume(result.output)
    deliver()
  }
  if (!terminal) {
    store(key, { status: 'lost', sessionId: undefined, directory, pid, tail })
    notify({ type: 'CI_MONITOR_UNAVAILABLE', key, reason: 'observer stream ended without a terminal result' })
  } else {
    store(key, { status: terminal.status === 'stopped' ? 'stopped' : 'finished', sessionId: undefined, directory, pid, tail, terminal })
  }
} catch (error) {
  store(key, { status: 'lost' })
  notify({ type: 'CI_MONITOR_UNAVAILABLE', key, reason: String(error).slice(-1000) })
}
```

The first yielded output exposes session, directory, and PID. Save them with the
cell ID, coordinator, and checkout in the observer note before the first push.
Cell `store` may stay invisible until the cell finishes; do not coordinate
shutdown through cross-cell `load`/`store`. Continue delegation after yielding.
`notify` arrives at the next coordinator boundary. Do not `wait`, assign a
watching agent, or broaden ordinary permissions.

When the stream ends without ever parsing a `CI_OBSERVER_RESULT` line, no
terminal evidence exists: store `lost`, not `finished`, and send
`CI_MONITOR_UNAVAILABLE` so the coordinator does not treat the stream's end as
closure. A parsed terminal result keeps its actual meaning (`stopped` or
`finished`); do not relabel a valid stop as loss, and never claim `finished`
from a missing or partial terminal record.

Without those host tools, report monitoring unavailable once and continue; never
poll or claim notifications from a background shell or file. Use another native
bridge only when its delivery contract is independently verified for this host.

When the shared [observer lifecycle](ci-monitor.md#own-one-observer) calls for
an explicit stop — human-judgment stop, cancellation, or coordinator
replacement — consume delivered failures first. Normal execution completion
uses the shared
[completion operation](ci-monitor.md#await-the-applicable-revision-at-completion)
instead; do not run this stop binding, `write_stdin` the stream PTY, or poll a
PID after that receipt. This stop binding never substitutes for that completion
operation. Copy it with the note's exact `directory`; do not `write_stdin` the
stream PTY. Confirm receipt and a finite local `ps` wait for the recorded PID;
never signal it. Report unread events and any still-unobserved coverage. The
finite process-exit check waits only for local shutdown, never for CI.

```js
const key = 'ci-watch-execution:OWNER/REPO:BRANCH:COORDINATOR'
try {
  const stop = await tools.exec_command({
    cmd: `node /ABSOLUTE/RESOLVED/SKILL/scripts/ci-mailbox.mjs stop ${directory}`,
    workdir: '/ABSOLUTE/VERIFIED/CHECKOUT_ROOT',
    yield_time_ms: 10000,
    max_output_tokens: 2000,
  })
  if (stop.session_id) throw new Error('observer stop still running')
  const line = String(stop.output).split('\n').find((row) => row.startsWith('CI_OBSERVER '))
  const receipt = JSON.parse(line.slice('CI_OBSERVER '.length))
  if (receipt.directory !== directory || receipt.terminal?.status !== 'stopped') throw new Error('observer stop did not confirm')
  text({ key, status: 'stopped', directory, terminal: receipt.terminal, pendingCi: receipt.terminal.coverage?.pendingCi ?? 'unobserved' })
} catch (error) {
  notify({ type: 'CI_MONITOR_UNAVAILABLE', key, reason: String(error).slice(-1000) })
}
```

Without handles, recover the observer note. Match coordinator/checkout and
`request.json` root, repository, branch, and execution mode, then evaluate the
stop binding. Read the terminal receipt and `result.json`. Missing/mismatched
identity means no guessed stop, newest-mailbox lookup, or replacement launch.
Older unidentified observers cannot be recovered. Stop errors, missing terminal
evidence, or unconfirmed exit mean unresolved shutdown; never force termination
or claim closure. Keep acknowledgment and repair unchanged. On resume, rearm
only absent, `stopped`, or `lost` observers after the old process ended; never
restart terminal `finished` observers. Normal and repair pushes retain the key,
session, directory, process, and cell. Changed HEAD/SHA never requires setup
again.
