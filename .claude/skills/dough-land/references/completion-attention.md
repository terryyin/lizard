# Completion attention

Dough Land and Story Wrap Up use this rule after their operations settle:

- On successful completion with nothing requiring attention, give no recap.
  Use only a completion marker the calling skill already requires, if any;
  create no new marker. Routine publication, successful refresh, default-checkout
  cleanup or refresh being not applicable, and already-absent cleanup alone
  require no attention message. A direct landing with no CI observation obligation
  does not acquire one or report uncertain coverage solely from its absence.
- Report a material concern, reminder, failure, unfinished step, lost or uncertain
  CI coverage required by this invocation, deferred or stopped refresh, or blocked,
  partial, or unverified retirement. Recorded process-finding IDs, and a process
  review that was unavailable or could not record its findings, are reminders.
  Give the useful facts, their consequence, and the next action, naming its
  responsible owner when known. Distinguish accepted publication from an
  unfinished later step so recovery repeats only unfinished work. Report a
  follow-up when it is a useful reminder, without recapping settled work.
- When retirement removed the checkout this session ran in, the session can no
  longer act. State that the work is finished and where it landed, then list any
  reminders. Write each reminder as its fact, consequence, and next action with
  its owner, for the developer to take up in a new session or by hand. Ask no
  question, request no decision from this session, and give no command for it to
  run next. Report a record removed so Git can recover it as a fact, such as
  "… stays recoverable at `<sha>`". While the checkout survives, because
  retirement was held, a step remains unfinished, or the work is local-only, the
  response may still ask for the input needed to continue.
- When the developer explicitly requests details, provide the requested facts.
  Keep operational evidence in command results, available conversation context,
  and its existing lasting homes; silence and a marker never prove completion
  or relax publication, observation, shutdown, recovery, or retirement gates.

Without supplied dashboard launch context, direct use makes no dashboard contact;
do not discover or invent a session to report to.

With supplied dashboard launch context, apply the shared
[dashboard completion operation](dashboard-completion.md) after final wording settles.
