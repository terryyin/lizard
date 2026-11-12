// The declarations in .planning/open-dough.json; file reading stays with callers.
import { isSafeRepositoryPath } from "./repository-path.mjs";

/** @typedef {import("./intent-location-selection.mjs").SourceDeclaration} SourceDeclaration */
/** @typedef {import("./intent-location-selection.mjs").LocationSelection & { readonly label?: string, readonly makeMe?: boolean, readonly pageObjects?: boolean }} DeclaredLocation */
/** @typedef {{ readonly kind: "absent" } | { readonly kind: "malformed" } | { readonly kind: "locations", readonly locations: DeclaredLocation[] }} DeclaredSettings */

/** @param {unknown} value @returns {readonly string[] | undefined} */
function includePatternsOf(value) {
  if (!Array.isArray(value)) return undefined;
  const patterns = [];
  for (const entry of value) {
    if (typeof entry !== "string" || !isSafeRepositoryPath(entry))
      return undefined;
    patterns.push(entry);
  }
  return patterns;
}

/** @param {unknown} value @returns {readonly SourceDeclaration[] | undefined} */
function sourceDeclarationsOf(value) {
  if (!Array.isArray(value) || value.length === 0) return undefined;
  const sources = [];
  for (const entry of value) {
    if (entry === null || typeof entry !== "object" || Array.isArray(entry))
      return undefined;
    const { include, syntax } = entry;
    const patterns = includePatternsOf(include);
    if (patterns === undefined || patterns.length === 0) return undefined;
    if (syntax === undefined) sources.push({ include: patterns });
    else if (typeof syntax === "string" && syntax.length > 0)
      sources.push({ include: patterns, syntax });
    else return undefined;
  }
  return sources;
}

// Invalid entries are dropped; sources, when present, alone selects. Absence
// retains Intent's existing not-configured answer; unreadable JSON is malformed.
/** @param {string} settings @returns {DeclaredSettings} */
export function declaredLocations(settings) {
  let parsed;
  try {
    parsed = JSON.parse(settings);
  } catch {
    return { kind: "malformed" };
  }
  if (parsed === null || typeof parsed !== "object" || Array.isArray(parsed))
    return { kind: "absent" };
  const intent = parsed.intent;
  if (intent === null || typeof intent !== "object" || Array.isArray(intent))
    return { kind: "absent" };
  const locations = intent.locations;
  if (!Array.isArray(locations) || locations.length === 0)
    return { kind: "absent" };
  /** @type {DeclaredLocation[]} */
  const declared = [];
  for (const entry of locations) {
    if (entry === null || typeof entry !== "object" || Array.isArray(entry))
      continue;
    const { path, kind, include, label, sources, makeMe, pageObjects } = entry;
    if (typeof path !== "string" || !isSafeRepositoryPath(path)) continue;
    const named = {
      path,
      ...(typeof label === "string" && label.trim() !== ""
        ? { label: label.trim() }
        : {}),
      ...(typeof makeMe === "boolean" ? { makeMe } : {}),
      ...(typeof pageObjects === "boolean" ? { pageObjects } : {}),
    };
    if ("sources" in entry) {
      const declarations = sourceDeclarationsOf(sources);
      if (declarations !== undefined)
        declared.push({ ...named, sources: declarations });
      continue;
    }
    if (typeof kind !== "string" || kind.length === 0) continue;
    const fields = { ...named, kind };
    if (!("include" in entry)) {
      declared.push(fields);
      continue;
    }
    const patterns = includePatternsOf(include);
    if (patterns !== undefined) declared.push({ ...fields, include: patterns });
  }
  return declared.length === 0
    ? { kind: "absent" }
    : { kind: "locations", locations: declared };
}
