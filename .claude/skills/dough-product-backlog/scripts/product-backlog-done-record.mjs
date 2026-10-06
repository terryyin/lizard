// What a done record is: the short published fact that one backlog entry was
// completed, kept beside the backlog after the work's spent history is
// deleted. It holds only facts that outlive cleanup: the work's identity and
// title, when `complete` ran, the developer configured in that workspace, and
// the execution agent, host, and model when the work had an execution agent
// profile. The directory, the file name each identity takes, the spelling,
// and the window a record stays for have one owner here. No filesystem, Git,
// or Node-only imports, so every reader of published records shares it.

import { agentHosts, agentNameOf } from "./product-backlog-agent-profile.mjs";

// Done records live beside the backlog, one file per completed identity.
export const doneRecordDirectory = "done";

// How long a done record stays: `complete` removes records completed more
// than this many days before it runs, and readers leave them out.
export const doneRecordWindowDays = 30;

const dayMilliseconds = 24 * 60 * 60 * 1000;

// Characters an identity keeps in its file name. `#` reads as `_`; any other
// character, `_` and `~` among them, is written as `~` and its UTF-8 bytes in
// hex, as is a leading `.`, so each identity has exactly one file name and no
// two identities share one.
const kept = /[A-Za-z0-9.-]/;
const encoder = new TextEncoder();

function escaped(character) {
  return [...encoder.encode(character)]
    .map((byte) => `~${byte.toString(16).toUpperCase().padStart(2, "0")}`)
    .join("");
}

// The file name (within the done-record directory) the record of `identity`
// takes. Completing the same identity again names the same file.
export function doneRecordFileName(identity) {
  const spelled = [...identity]
    .map((character, index) => {
      if (character === "#") return "_";
      if (index === 0 && character === ".") return escaped(character);
      return kept.test(character) ? character : escaped(character);
    })
    .join("");
  return `${spelled}.json`;
}

// The record's path relative to the backlog's directory.
export function doneRecordPath(identity) {
  return `${doneRecordDirectory}/${doneRecordFileName(identity)}`;
}

// Whether a file in the done-record directory can be a done record by its
// name alone; its text decides whether it is a readable one.
export function isDoneRecordFileName(fileName) {
  return /^[A-Za-z0-9._~-]+\.json$/.test(fileName) && !fileName.startsWith(".");
}

const nonEmptyText = (value) => typeof value === "string" && value !== "";

function recordFactsError({
  identity,
  title,
  completedAt,
  developer,
  agent,
  host,
  model,
}) {
  if (!nonEmptyText(identity)) return "done record requires an identity";
  if (!nonEmptyText(title)) return "done record requires a title";
  if (
    !nonEmptyText(completedAt) ||
    Number.isNaN(Date.parse(completedAt)) ||
    new Date(completedAt).toISOString() !== completedAt
  )
    return "done record requires a UTC ISO completion time";
  if (developer !== undefined && !nonEmptyText(developer))
    return "developer must be non-empty text when recorded";
  if (agent !== undefined && agentNameOf(agent) === undefined)
    return `unknown agent: ${agent}`;
  if (agent === undefined && (host !== undefined || model !== undefined))
    return "host and model are recorded only with an agent";
  if (host !== undefined && !agentHosts.includes(host))
    return `host must be one of ${agentHosts.join(", ")}`;
  if (model !== undefined && !nonEmptyText(model))
    return "model must be non-empty text when recorded";
  return undefined;
}

/**
 * @param {{ identity: string, title: string, completedAt: string,
 *   developer?: string | undefined, agent?: string | undefined,
 *   host?: string | undefined, model?: string | undefined }} facts
 */
export function renderDoneRecord(facts) {
  const error = recordFactsError(facts);
  if (error) throw new Error(error);
  const { identity, title, completedAt, developer, agent, host, model } = facts;
  const record = {
    schemaVersion: 1,
    identity,
    title,
    completedAt,
    ...(developer === undefined ? {} : { developer }),
    ...(agent === undefined ? {} : { agent }),
    ...(host === undefined ? {} : { host }),
    ...(model === undefined ? {} : { model }),
  };
  return `${JSON.stringify(record, null, 2)}\n`;
}

// Reads published record text back into the facts renderDoneRecord takes:
// { ok: true, record } or { ok: false, error }. Unrecorded facts stay absent.
export function parseDoneRecord(text) {
  let data;
  try {
    data = JSON.parse(text);
  } catch {
    return { ok: false, error: "done record is not JSON" };
  }
  if (data === null || typeof data !== "object" || data.schemaVersion !== 1)
    return { ok: false, error: "done record schemaVersion must be 1" };
  const { identity, title, completedAt, developer, agent, host, model } = data;
  const facts = { identity, title, completedAt, developer, agent, host, model };
  const error = recordFactsError(facts);
  if (error) return { ok: false, error };
  return {
    ok: true,
    record: Object.fromEntries(
      Object.entries(facts).filter(([, value]) => value !== undefined),
    ),
  };
}

// Reads the record published as `fileName` as parseDoneRecord does, and also
// refuses text whose identity belongs to another file name.
export function parseDoneRecordFile(fileName, text) {
  const read = parseDoneRecord(text);
  if (read.ok && doneRecordFileName(read.record.identity) !== fileName)
    return { ok: false, error: "done record names another identity" };
  return read;
}

// Whether a record completed at `completedAt` is still inside the window at
// `now` (a Date): completed no more than doneRecordWindowDays before it.
export function isWithinDoneWindow(completedAt, now) {
  return (
    now.getTime() - Date.parse(completedAt) <=
    doneRecordWindowDays * dayMilliseconds
  );
}
