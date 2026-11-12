// One source-membership and syntax rule for Intent and installed guidance.
import { minimatch } from "./minimatch.bundle.mjs";

/** @typedef {{ readonly patterns?: readonly string[], readonly hint?: string, readonly extensions?: readonly string[] }} SourceEntry */
/** @typedef {{ readonly include: readonly string[], readonly syntax?: string | undefined }} SourceDeclaration */
/** @typedef {{ readonly path: string, readonly kind: string, readonly include?: readonly string[] | undefined } | { readonly path: string, readonly sources: readonly SourceDeclaration[] }} LocationSelection */

/** @type {Record<string, { extensions: readonly string[] }>} */
const intentSourceKinds = {
  gherkin: { extensions: [".feature"] },
  typescript: { extensions: [".ts", ".tsx"] },
  javascript: { extensions: [".js", ".mjs", ".cjs"] },
  java: { extensions: [".java"] },
  python: { extensions: [".py"] },
  shell: { extensions: [".sh", ".bash"] },
};

// POSIX and case-sensitive on every host, including macOS. Preserve the
// minimatch semantics used by Intent rather than Node's host-dependent glob.
const globOptions = {
  nonegate: true,
  nocomment: true,
  windowsPathsNoEscape: true,
  optimizationLevel: 2,
  platform: "linux",
};

/** @param {LocationSelection} location @returns {readonly SourceEntry[] | undefined} */
export function sourceEntriesOf(location) {
  if ("sources" in location) {
    return location.sources.map(({ include, syntax }) => ({
      patterns: include,
      ...(syntax === undefined ? {} : { hint: syntax }),
    }));
  }
  const extensions = intentSourceKinds[location.kind]?.extensions;
  if (extensions === undefined) return undefined;
  return [
    {
      extensions,
      hint: location.kind,
      ...(location.include === undefined ? {} : { patterns: location.include }),
    },
  ];
}

/** @param {SourceEntry} entry @param {string} relativePath */
function entrySelects(entry, relativePath) {
  if (
    entry.extensions !== undefined &&
    !entry.extensions.some((extension) => relativePath.endsWith(extension))
  )
    return false;
  return (
    entry.patterns === undefined ||
    entry.patterns.some((pattern) =>
      minimatch(relativePath, pattern, globOptions),
    )
  );
}

/** @param {string} relativePath @param {readonly SourceEntry[]} entries @returns {SourceEntry | undefined} */
export function matchingSourceEntry(relativePath, entries) {
  return entries.find((entry) => entrySelects(entry, relativePath));
}

/** @param {string} filePath @param {LocationSelection} location @returns {SourceEntry | undefined} */
export function sourceEntryOfRepositoryPath(filePath, location) {
  if (!filePath.startsWith(`${location.path}/`)) return undefined;
  const entries = sourceEntriesOf(location);
  return entries === undefined
    ? undefined
    : matchingSourceEntry(filePath.slice(location.path.length + 1), entries);
}

/** @param {string} filePath @param {LocationSelection} location */
export function locationSelectsRepositoryPath(filePath, location) {
  return sourceEntryOfRepositoryPath(filePath, location) !== undefined;
}
