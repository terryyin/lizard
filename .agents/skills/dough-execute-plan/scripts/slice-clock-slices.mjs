// The slices of a slice clock record: their entries and their overruns, and
// what every clock command reports about a slice or a diagnosis round.
// `slice-clock-record.mjs` describes the record and stores it.
import {
  elapsedMs,
  minutes,
  reachesLimit,
  remainingMs,
  sliceLimitMinutes,
} from "./slice-clock-active-time.mjs";
import {
  diagnosisLabel,
  roundEndMessage,
  roundEndSeen,
  roundFields,
  roundOf,
} from "./slice-clock-diagnosis.mjs";

// Whether the slice or diagnosis is the one running in the checkout.
export function isRunning(record, named) {
  const { identity, slice, diagnosis } = named;
  const { running } = record;
  return (
    running?.identity === identity &&
    running.slice === slice &&
    running.diagnosis === diagnosis &&
    (named.round === undefined ||
      sliceOf(record, named) === sliceOf(record, running))
  );
}

// The slice's entry, or a diagnosis's latest round; undefined when the record
// has none.
export function sliceOf(record, named) {
  if (named.diagnosis !== undefined) return roundOf(record, named);
  return record.stories[named.identity]?.slices[named.slice];
}

// How reports and errors name the slice or diagnosis.
export function labelOf(named) {
  if (named.diagnosis !== undefined) return diagnosisLabel(named);
  return `slice ${named.slice} of ${named.identity}`;
}

// Moves the entry of slice `from` to `named`, so a renumbered slice keeps its
// elapsed time and its overrun.
export function continueSlice(record, { identity, slice }, from) {
  const slices = record.stories[identity]?.slices ?? {};
  if (slices[from] === undefined)
    throw new Error(`no record of slice ${from} of ${identity} to continue`);
  if (from === slice) return;
  if (slices[slice] !== undefined)
    throw new Error(`slice ${slice} of ${identity} already has a record`);
  slices[slice] = slices[from];
  delete slices[from];
}

// Drops the entry of slice `from` when it has overrun, so a slice split from
// it can reuse its key from zero. The story keeps the overrun.
export function dropOverrunSlice(record, identity, from) {
  const slices = record.stories[identity]?.slices ?? {};
  if (slices[from]?.overrunDetectedAt !== undefined) delete slices[from];
}

// Ends the previous session at its last clock command without observing a
// new overrun.
export function settleEndedSession(record) {
  if (!record.running) return;
  const entry = sliceOf(record, record.running);
  for (const interval of [...entry.periods, ...entry.commands])
    interval.endedAt ??= record.lastClockCommandAt;
  Object.assign(record, { running: null });
}

// The slice's entry, added when the record has none.
export function ensureSlice(record, { identity, slice }) {
  const { stories } = record;
  stories[identity] ??= { slices: {} };
  const { slices } = stories[identity];
  slices[slice] ??= { periods: [], commands: [] };
  return slices[slice];
}

// Whether the slice running in the checkout is at or past the limit. It
// changes nothing, so a reader can ask before it takes the record to report.
export function runningSliceReachedLimit(record, now) {
  if (!record.running) return false;
  const entry = sliceOf(record, record.running);
  return entry !== undefined && reachesLimit(elapsedMs(entry, now));
}

// Counts the slice's overrun in its story the first time it is seen at or
// past the limit, and returns whether the slice has overrun.
function overrunSeen(record, named, elapsed, now) {
  const story = record.stories[named.identity];
  const entry = story.slices[named.slice];
  if (entry.overrunDetectedAt !== undefined) return true;
  if (!reachesLimit(elapsed)) return false;
  entry.overrunDetectedAt = now;
  story.overruns = [
    ...(story.overruns ?? []),
    { slice: named.slice, detectedAt: now },
  ];
  return true;
}

// What every clock command reports about one slice or diagnosis round.
// Seeing it at the limit records its end, so report inside `updateRecord`.
export function sliceReport(record, named, now) {
  const entry = sliceOf(record, named);
  const elapsed = elapsedMs(entry, now);
  const round = named.diagnosis !== undefined;
  const overrun = round
    ? roundEndSeen(entry, elapsed, now)
    : overrunSeen(record, named, elapsed, now);
  const report = {
    identity: named.identity,
    ...(round ? roundFields(record, named, now) : { slice: named.slice }),
    state: isRunning(record, named) ? "running" : "paused",
    elapsedMinutes: minutes(elapsed),
    remainingMinutes: minutes(remainingMs(elapsed)),
    limitMinutes: sliceLimitMinutes,
    overrun,
    storyOverruns: record.stories[named.identity].overruns?.length ?? 0,
  };
  if (!overrun) return report;
  const reached = `reached the ${sliceLimitMinutes}-minute limit: ${report.elapsedMinutes} active minutes`;
  return {
    ...report,
    message: round
      ? roundEndMessage(report, reached)
      : `${labelOf(named)} ${reached}`,
  };
}

// The slice's overrun message for `agent`, once per overrun: undefined while
// the slice is under the limit and after the agent has been told. Seeing the
// slice at the limit records its overrun, as `sliceReport` does.
export function overrunMessageFor(record, named, agent, now) {
  const report = sliceReport(record, named, now);
  const entry = sliceOf(record, named);
  const told = entry.overrunReportedTo ?? [];
  if (!report.overrun || told.includes(agent)) return undefined;
  entry.overrunReportedTo = [...told, agent];
  return report.message;
}
