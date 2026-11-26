// The diagnosis rounds of a slice clock record. A diagnosis whose cause is
// unknown runs in rounds of the slice limit, each started by its own `start`.
// The rounds are kept under the story, so every agent and resume in the
// checkout continues the same count; nothing resets it. A round reaching the
// limit ends the round: it reports an overrun but adds no story overrun.
// Starting a round beyond the authorized total is refused; only a human's
// authorization raises the total. Whether to continue is the agent's call.
import {
  elapsedMs,
  minutes,
  reachesLimit,
} from "./slice-clock-active-time.mjs";

export const defaultRoundsAuthorized = 3;

// How reports and errors name the diagnosis.
export function diagnosisLabel({ identity, diagnosis }) {
  return `diagnosis ${diagnosis} of ${identity}`;
}

function diagnosisOf(record, { identity, diagnosis }) {
  return record.stories[identity]?.diagnoses?.[diagnosis];
}

// The round `named` pins, or else the diagnosis's latest round; undefined
// when the diagnosis has none.
export function roundOf(record, named) {
  const rounds = diagnosisOf(record, named)?.rounds ?? [];
  return named.round === undefined ? rounds.at(-1) : rounds[named.round - 1];
}

// `named` pinned to its current round, so a run that outlives the round
// still ends its interval in that round.
export function pinnedRound(record, named) {
  if (named.diagnosis === undefined) return named;
  return { ...named, round: diagnosisOf(record, named).rounds.length };
}

function cumulativeMinutes({ rounds }, now) {
  return minutes(rounds.reduce((sum, round) => sum + elapsedMs(round, now), 0));
}

// Starts the diagnosis's next round at `now`, after raising its authorized
// total to `authorized` when a human authorized more.
export function startRound(record, named, authorized, now) {
  const { identity, diagnosis } = named;
  const { stories } = record;
  stories[identity] ??= { slices: {} };
  const story = stories[identity];
  story.diagnoses ??= {};
  const entry = (story.diagnoses[diagnosis] ??= {
    roundsAuthorized: defaultRoundsAuthorized,
    rounds: [],
  });
  const name = diagnosisLabel(named);
  if (authorized !== undefined) {
    if (authorized < entry.roundsAuthorized)
      throw new Error(
        `${name} has ${entry.roundsAuthorized} authorized rounds; --rounds-authorized only raises them`,
      );
    entry.roundsAuthorized = authorized;
  }
  if (entry.rounds.length >= entry.roundsAuthorized)
    throw new Error(
      `${name} used its ${entry.roundsAuthorized} authorized rounds: ${cumulativeMinutes(entry, now)} cumulative active minutes; a human may authorize more with --rounds-authorized`,
    );
  entry.rounds.push({
    periods: [{ startedAt: now, endedAt: null }],
    commands: [],
  });
}

// Whether the round has ended, marking it the first time it is seen at the
// limit. A round end is the design, not a sizing failure: no story overrun.
export function roundEndSeen(round, elapsed, now) {
  if (round.overrunDetectedAt === undefined && reachesLimit(elapsed))
    Object.assign(round, { overrunDetectedAt: now });
  return round.overrunDetectedAt !== undefined;
}

// What a report adds about the diagnosis `named` names.
export function roundFields(record, named, now) {
  const entry = diagnosisOf(record, named);
  return {
    diagnosis: named.diagnosis,
    round: named.round ?? entry.rounds.length,
    roundsAuthorized: entry.roundsAuthorized,
    cumulativeMinutes: cumulativeMinutes(entry, now),
  };
}

// The message of a report whose round reached the limit.
export function roundEndMessage(report, reached) {
  const { round, roundsAuthorized } = report;
  return `round ${round} of ${diagnosisLabel(report)} ${reached}; round ${round} of ${roundsAuthorized} authorized, ${report.cumulativeMinutes} cumulative active minutes`;
}
