// Git mechanics: who published a Taken claim. Identical Taken text is not
// ownership. Provenance is the commit that introduced the identity, read from
// its Claim-Publisher trailer, compared with retained execution context.
import {
  parseBacklog,
  takenHeading,
} from "../../dough-product-backlog/scripts/product-backlog-document.mjs";
import { git } from "./publication-git.mjs";

export const backlogPath = ".planning/PRODUCT-BACKLOG.md";

// An admission claims accepted work that was never queued; its trailers are
// the same, so ownership is read the same way.
export function claimCommitMessage(identity, publisherId, admitted = false) {
  const subject = admitted ? "Admit accepted work" : "Take queued work";
  return `${subject}: ${identity}\n\nClaim-Identity: ${identity}\nClaim-Publisher: ${publisherId}\n`;
}

export function trailers(message) {
  const publisher = message.match(/^Claim-Publisher: ([^\n]+)$/m)?.[1];
  const identity = message.match(/^Claim-Identity: ([^\n]+)$/m)?.[1];
  return { publisher, identity };
}

export function takenIdentities(source) {
  return parseBacklog(source)
    .entries.filter((entry) => entry.list === takenHeading)
    .map((entry) => entry.identity);
}

// Text of `path` at `rev`, or null when `rev` holds no such file.
export async function fileAt(cwd, rev, path) {
  try {
    return (await git(cwd, "show", `${rev}:${path}`)).stdout;
  } catch {
    return null;
  }
}

export async function isAncestor(cwd, ancestor, descendant) {
  try {
    await git(cwd, "merge-base", "--is-ancestor", ancestor, descendant);
    return true;
  } catch (error) {
    if (error.code === 1) return false;
    throw error;
  }
}

// The commit that moved `identity` into Taken on `ref`, plus its trailer.
// Null when the identity is not Taken at that ref.
export async function claimProvenance(cwd, ref, identity, path = backlogPath) {
  const tip = await fileAt(cwd, ref, path);
  if (tip === null || !takenIdentities(tip).includes(identity)) return null;
  const shas = (await git(cwd, "rev-list", "--first-parent", ref)).stdout
    .trim()
    .split("\n")
    .filter(Boolean);
  for (const sha of shas) {
    const here = await fileAt(cwd, sha, path);
    if (here === null || !takenIdentities(here).includes(identity)) continue;
    const parent = await fileAt(cwd, `${sha}^`, path);
    if (parent !== null && takenIdentities(parent).includes(identity)) continue;
    const message = (await git(cwd, "log", "-1", "--format=%B", sha)).stdout;
    return { sha, ...trailers(message) };
  }
  return { sha: null, publisher: undefined, identity: undefined };
}

// "absent" | "owned" | "other" | "ambiguous"
export function classifyOwnership({
  publisherId,
  candidateSha,
  candidateIsAncestor,
  provenance,
}) {
  if (!provenance) return "absent";
  if (!provenance.publisher || !publisherId) return "ambiguous";
  if (provenance.publisher !== publisherId) return "other";
  if (
    (candidateSha && provenance.sha === candidateSha) ||
    candidateIsAncestor === true
  ) {
    return "owned";
  }
  return "ambiguous";
}

export const remoteOf = (request) => request.remote ?? "origin";
export const targetOf = (request) => request.target ?? "main";
export const remoteRef = (request) =>
  `${remoteOf(request)}/${targetOf(request)}`;
export const pathOf = (request) => request.backlogPath ?? backlogPath;

export function stopped(status, fields) {
  return { ok: false, status, implemented: false, ...fields };
}

// A stop for a source read that refused the selected work: the error names
// its own stop status and fields, such as a reconciliation conflict, or is
// an ordinary source refusal.
export function sourceStopped(error, fields) {
  return stopped(error.status ?? "source-refused", {
    ...error.fields,
    ...fields,
  });
}
