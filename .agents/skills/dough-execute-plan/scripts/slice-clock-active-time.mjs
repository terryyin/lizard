// The active time of one slice clock entry, a slice or a diagnosis round:
// the wall time of its periods minus the time inside commands run through the
// clock, overlaps excluded once, against the 15-minute limit.
export const sliceLimitMinutes = 15;

const minuteMs = 60_000;

// Intervals closed at `now`, sorted and merged so overlaps count once.
function union(intervals, now) {
  const merged = [];
  const closed = intervals
    .map(({ startedAt, endedAt }) => [startedAt, endedAt ?? now])
    .sort((a, b) => a[0] - b[0]);
  for (const [start, end] of closed) {
    const last = merged.at(-1);
    if (last !== undefined && start <= last[1])
      last[1] = Math.max(last[1], end);
    else merged.push([start, end]);
  }
  return merged;
}

// Active time: the wall time of the entry's periods minus the union of its
// command intervals inside them.
export function elapsedMs(entry, now) {
  const commands = union(entry.commands, now);
  let total = 0;
  for (const [start, end] of union(entry.periods, now)) {
    total += end - start;
    for (const [from, to] of commands)
      total -= Math.max(0, Math.min(end, to) - Math.max(start, from));
  }
  return total;
}

// Whether `elapsed` active time is at or past the limit.
export function reachesLimit(elapsed) {
  return elapsed >= sliceLimitMinutes * minuteMs;
}

// `ms` in minutes, to one decimal place, as reports give them.
export function minutes(ms) {
  return Math.round((ms / minuteMs) * 10) / 10;
}

// The active time left before the limit.
export function remainingMs(elapsed) {
  return Math.max(0, sliceLimitMinutes * minuteMs - elapsed);
}
