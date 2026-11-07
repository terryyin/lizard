// Whether one GitHub request owner (`./github-request-owner.mjs`) may ask
// GitHub now, for each of GitHub's rate-limit resources
// (`./github-request-admission.mjs`), and what it keeps of each rate
// limit's wait where its replacement finds it: one `hold.RESOURCE.json` in
// its endpoint directory (`./github-request-owner-endpoint.mjs`) while a
// wait stands or asking still resumes after one. A wait is stored before
// any answer tells of it (`./github-request-calls.mjs`), so an owner that is
// ended, however, leaves every wait a consumer heard of to the owner started
// next, which begins with the waits it finds. Only times and counts are
// kept: no login, route or answer.
import {
  closeSync,
  fsyncSync,
  openSync,
  readdirSync,
  readFileSync,
  renameSync,
  rmSync,
  writeSync,
} from "node:fs";
import { join } from "node:path";
import { ReadAdmission } from "./github-request-admission.mjs";
import { longestDirectedWaitSeconds } from "./github-request-rules.mjs";

/** @import { GhRequestFailure } from "./github-request-answer.mjs" */

// What an owner knows of one resource's wait: the time before which nothing
// is asked, how many undirected limits were met in a row, and whether one
// request still goes first.
/**
 * @typedef {{
 *   readonly resumesAtMs: number;
 *   readonly backoffStep: number;
 *   readonly resuming: boolean;
 * }} KeptHold
 */

const version = 1;
const holdName = /^hold\.([a-z]+)\.json$/;
const draftName = /^hold\.[a-z]+\.json\.\d+\.draft$/;

const written = (hold) => JSON.stringify({ version, ...hold });

// Stores `text` as `name` in `directory`, whole or not at all for whoever
// reads it, and returns only once the file and its name would outlast this
// machine stopping: a draft written and flushed, renamed into place, and
// the directory flushed.
function storedDurably(directory, name, text) {
  const draft = join(directory, `${name}.${process.pid}.draft`);
  try {
    const file = openSync(draft, "w", 0o600);
    try {
      writeSync(file, text);
      fsyncSync(file);
    } finally {
      closeSync(file);
    }
    renameSync(draft, join(directory, name));
  } catch (error) {
    rmSync(draft, { force: true });
    throw error;
  }
  const holding = openSync(directory, "r");
  try {
    fsyncSync(holding);
  } finally {
    closeSync(holding);
  }
}

function readable(file) {
  try {
    return readFileSync(file, "utf8");
  } catch {
    return "";
  }
}

// The wait `text` records. One naming a later time than any wait may last
// ends when the longest would; one that cannot be read is taken as a wait
// already over, after which one request still goes first.
/** @returns {KeptHold} */
function read(text, nowMs) {
  try {
    const { resumesAtMs, backoffStep, resuming, ...rest } = JSON.parse(text);
    if (
      rest.version === version &&
      Number.isFinite(resumesAtMs) &&
      Number.isSafeInteger(backoffStep) &&
      backoffStep >= 0 &&
      typeof resuming === "boolean"
    )
      return {
        resumesAtMs: Math.min(
          resumesAtMs,
          nowMs + longestDirectedWaitSeconds * 1000,
        ),
        backoffStep,
        resuming,
      };
  } catch {
    // Not a record this version wrote.
  }
  return { resumesAtMs: 0, backoffStep: 0, resuming: true };
}

export class KeptAdmissions {
  #directory;

  #backoffBaseMs;

  // Each resource asked so far or found with a wait kept, and what stands
  // stored for it, as it was written.
  /** @type {Map<string, ReadAdmission>} */
  #admissions = new Map();

  /** @type {Map<string, string>} */
  #stored = new Map();

  // Begins with every wait an earlier owner left in `directory`. A draft
  // left by an owner ended while storing is no wait -- nothing told of it
  // -- and is removed. `backoffBaseMs` is the first wait after a rate limit
  // that directs none, doubling while the limit continues.
  /**
   * @param {string} directory
   * @param {number} backoffBaseMs
   */
  constructor(directory, backoffBaseMs) {
    this.#directory = directory;
    this.#backoffBaseMs = backoffBaseMs;
    for (const name of readdirSync(directory)) {
      const file = join(directory, name);
      const [, resource] = holdName.exec(name) ?? [];
      if (draftName.test(name)) rmSync(file, { force: true });
      if (resource === undefined) continue;
      const text = readable(file);
      const hold = read(text, Date.now());
      if (written(hold) === text) this.#stored.set(resource, text);
      this.#admissions.set(resource, new ReadAdmission(backoffBaseMs, hold));
    }
  }

  // The admission of one resource's requests.
  /** @param {string} resource */
  of(resource) {
    let admission = this.#admissions.get(resource);
    if (admission === undefined) {
      admission = new ReadAdmission(this.#backoffBaseMs);
      this.#admissions.set(resource, admission);
    }
    return admission;
  }

  // Why nothing of some resource may be asked of GitHub now, when a rate
  // limit's wait stands.
  /** @param {number} nowMs */
  heldBack(nowMs) {
    for (const admission of this.#admissions.values()) {
      const refused = admission.heldBack(nowMs);
      if (refused !== undefined) return refused;
    }
    return undefined;
  }

  // Why a call of `resource` may not be asked now. A wait is told of only
  // once it is kept: until it is, the call is refused as failed, and still
  // not asked.
  /**
   * @param {string} resource
   * @returns {GhRequestFailure | undefined}
   */
  refusal(resource) {
    const refused = this.of(resource).heldBack(Date.now());
    return refused === undefined || this.keep(resource)
      ? refused
      : { kind: "failed" };
  }

  // Stores what `resource`'s admission knows of a wait, or removes it once
  // that is nothing; unchanged, it is not written again. Whether it stands
  // stored: false leaves the earlier record, and the next call stores again.
  /** @param {string} resource */
  keep(resource) {
    const hold = this.of(resource).kept;
    const text = hold === undefined ? undefined : written(hold);
    if (this.#stored.get(resource) === text) return true;
    const name = `hold.${resource}.json`;
    try {
      if (text === undefined) {
        rmSync(join(this.#directory, name), { force: true });
        this.#stored.delete(resource);
      } else {
        storedDurably(this.#directory, name, text);
        this.#stored.set(resource, text);
      }
      return true;
    } catch {
      return false;
    }
  }
}
