// Waits in this checkout until a remote publishes a change to a watched ref,
// or to a path on one named REF:PATH, or until the deadline. It prints
// nothing while the watched objects are unchanged and exactly one JSON result
// line when the wait ends.
//
// A wait that ends with a publication retains what it last observed in the
// checkout's Git directory. A later run naming the same remote, refs and
// --deadline continues that wait: it observes at once and reports every change
// since that observation, including one made while the result was judged. Any
// other run takes its own first observation. A wait reads FETCH_HEAD after each
// fetch, so nothing else fetches in its checkout while it runs: the rerun
// starts after the judgment rather than during it.
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { setTimeout as delay } from "node:timers/promises";
import { parseArgs } from "node:util";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { git } from "./publication-git.mjs";

const usage =
  "Usage: node wait-for-publication.mjs --remote REMOTE --ref REF[:PATH] [--ref REF[:PATH]]... [--duration-minutes MINUTES | --deadline ISO_TIME]";
const flags = ["remote", "ref", "duration-minutes", "deadline"];
const defaultDurationMinutes = 60;
const pollIntervalMs = 60_000;

function positive(text, flag) {
  const value = Number(text);
  if (!(Number.isFinite(value) && value > 0))
    throw new Error(`${flag} needs a positive number, not ${text}`);
  return value;
}

// REF watches the ref's own object; REF:PATH watches that path on the ref.
function watchedItem(named) {
  const colon = named.indexOf(":");
  return colon < 0
    ? { ref: named }
    : { ref: named.slice(0, colon), path: named.slice(colon + 1) };
}

// The wait the arguments describe, with its deadline as a time.
function waitRequest(argv) {
  const { values } = parseArgs({
    args: argv,
    options: Object.fromEntries(
      flags.map((flag) => [flag, { type: "string", multiple: true }]),
    ),
  });
  const once = (flag) => {
    if (values[flag]?.length > 1) throw new Error(`name --${flag} once`);
    return values[flag]?.[0];
  };
  const remote = once("remote");
  if (remote === undefined || values.ref === undefined)
    throw new Error("name one --remote and at least one --ref");
  const duration = once("duration-minutes");
  const deadline = once("deadline");
  if (duration !== undefined && deadline !== undefined)
    throw new Error("name --duration-minutes or --deadline, not both");
  const deadlineMs =
    deadline === undefined
      ? Date.now() +
        positive(duration ?? defaultDurationMinutes, "--duration-minutes") *
          60_000
      : Date.parse(deadline);
  if (Number.isNaN(deadlineMs))
    throw new Error(`--deadline needs an ISO time, not ${deadline}`);
  return {
    remote,
    watched: values.ref.map(watchedItem),
    deadlineMs,
  };
}

// The object the fetched ref holds at `path`, or the ref's own object; null
// for a path the ref does not hold.
async function fetchedObject(checkout, path) {
  const name = path === undefined ? "FETCH_HEAD" : `FETCH_HEAD:${path}`;
  try {
    const found = await git(checkout, "rev-parse", "--verify", "--quiet", name);
    return found.stdout.trim();
  } catch (error) {
    if (error.code === 1 && path !== undefined) return null;
    throw error;
  }
}

// Every watched object as the remote holds it now, fetching each ref once per
// run of items that name it.
async function observe(checkout, { remote, watched }) {
  const observed = [];
  let fetched;
  for (const item of watched) {
    if (item.ref !== fetched) {
      try {
        await git(checkout, "fetch", "--quiet", remote, item.ref);
      } catch (error) {
        const reason = (error.stderr || error.message).trim();
        throw new Error(`cannot read ${item.ref} from ${remote}: ${reason}`, {
          cause: error,
        });
      }
      fetched = item.ref;
    }
    observed.push({
      ...item,
      object: await fetchedObject(checkout, item.path),
    });
  }
  return observed;
}

function changesBetween(before, after) {
  return after.flatMap(({ object, ...watched }, index) =>
    object === before[index].object
      ? []
      : [{ ...watched, before: before[index].object, after: object }],
  );
}

// The file, per worktree, holding the last observation of a wait that ended
// with a publication.
async function retainedPath(checkout) {
  const found = await git(
    checkout,
    "rev-parse",
    "--git-path",
    "publication-wait.json",
  );
  return resolve(checkout, found.stdout.trim());
}

// The wait a retained observation belongs to.
function waitKey({ remote, watched }, deadline) {
  return JSON.stringify({ remote, watched, deadline });
}

// The observation retained for this wait; null when another wait, or none
// readable, was retained.
async function retainedObservation(file, key, { watched }) {
  try {
    const { key: retainedKey, observed } = JSON.parse(
      await readFile(file, "utf8"),
    );
    return retainedKey === key && observed.length === watched.length
      ? observed
      : null;
  } catch {
    return null;
  }
}

// Resolves with the one result of the wait. A first observation that fails
// rejects; a later one, or a continued wait's, is tried again at the next
// interval.
async function waitForPublication(checkout, request) {
  const { remote, deadlineMs } = request;
  const deadline = new Date(deadlineMs).toISOString();
  const file = await retainedPath(checkout);
  const key = waitKey(request, deadline);
  const retained = await retainedObservation(file, key, request);
  const start = retained ?? (await observe(checkout, request));
  let pause = retained === null;
  for (;;) {
    const remaining = deadlineMs - Date.now();
    if (remaining <= 0) return { result: "deadline", remote, deadline };
    if (pause) await delay(Math.min(pollIntervalMs, remaining));
    pause = true;
    const now = await observe(checkout, request).catch(() => start);
    const changes = changesBetween(start, now);
    if (changes.length > 0) {
      // Unretained, the next run takes its own first observation.
      await writeFile(file, JSON.stringify({ key, observed: now })).catch(
        () => {},
      );
      return { result: "publication", remote, changes, deadline };
    }
  }
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  let request;
  try {
    request = waitRequest(process.argv.slice(2));
  } catch (error) {
    process.stderr.write(`${error.message}\n${usage}\n`);
    process.exit(2);
  }
  try {
    const result = await waitForPublication(process.cwd(), request);
    process.stdout.write(`${JSON.stringify(result)}\n`);
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exit(1);
  }
}
