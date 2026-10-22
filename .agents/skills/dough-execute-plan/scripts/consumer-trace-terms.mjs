// Effect terms derived from the change itself: the files a working tree
// changes against a base revision, the identifiers its changed lines declare,
// each changed module's import-path forms and the literals its removed lines
// held. Derivation is lexical; behavior the diff cannot carry stays supplied.
import { readFileSync } from "node:fs";
import { basename, dirname, extname, resolve } from "node:path";

// Prose lines hold `Word:` labels and sentences, not declarations.
const PROSE = /\.(md|markdown|txt|rst)$/i;
const IDENTIFIER = "[A-Za-z_$][\\w$]*";
const DECLARATION = new RegExp(
  `\\b(?:function\\*?|class|const|let|var|type|interface|enum)\\s+(${IDENTIFIER})`,
  "g",
);
const EXPORT_LIST = /\bexport\s+(?:type\s+)?\{([^}]*)\}/g;
const OBJECT_KEY = new RegExp(
  `^\\s*(?:readonly\\s+)?(?:(${IDENTIFIER})|"([\\w$-]+)"|'([\\w$-]+)')\\??\\s*:(?!:)`,
);
// Every literal is consumed in order, so a short one such as "-C" cannot
// leave its closing quote to open a bogus literal spanning code.
const LITERAL = /"((?:[^"\\]|\\.)*)"|'((?:[^'\\]|\\.)*)'|`([^`$\\]*)`/g;
// A removed literal shorter than this names too little to search for.
const MIN_LITERAL = 3;
// A one- or two-letter identifier matches almost any file as a substring.
const MIN_IDENTIFIER = 3;

// Module-scope declarations (an unindented line) and exports are what other
// files can consume; an indented `const` is a local. Keys count on any line,
// since a contract's fields are indented.
export function declaredIdentifiers(line) {
  const names = [];
  const moduleScope = !/^\s/.test(line) || /\bexport\b/.test(line);
  if (moduleScope)
    for (const match of line.matchAll(DECLARATION)) names.push(match[1]);
  for (const match of line.matchAll(EXPORT_LIST))
    for (const item of match[1].split(","))
      names.push(
        ...item
          .trim()
          .split(/\s+as\s+/)
          .filter(Boolean),
      );
  const key = line.match(OBJECT_KEY);
  if (key) names.push(key[1] ?? key[2] ?? key[3]);
  return names.filter((name) => name && name.length >= MIN_IDENTIFIER);
}

export function stringLiterals(line) {
  return [...line.matchAll(LITERAL)].map((match) =>
    (match[1] ?? match[2] ?? match[3]).trim(),
  );
}

// The forms another module uses to import this one. An index module is
// imported through its directory.
export function importPathForms(path) {
  const extension = extname(path);
  if (!extension) return [];
  const bare = path.slice(0, -extension.length);
  if (basename(bare) === "index") {
    const directory = dirname(bare);
    return directory === "." ? [] : [directory, basename(directory)];
  }
  return [bare, basename(bare)];
}

function hunkLines(patch) {
  const lines = [];
  let removed = 0;
  for (const line of patch.split("\n")) {
    const header = line.match(/^@@ -(\d+)(?:,\d+)? \+\d+(?:,\d+)? @@/);
    if (header) removed = Number(header[1]);
    else if (line.startsWith("-") && !line.startsWith("--- "))
      lines.push({ side: "removed", line: removed++, text: line.slice(1) });
    else if (line.startsWith("+") && !line.startsWith("+++ "))
      lines.push({ side: "added", text: line.slice(1) });
  }
  return lines;
}

// The checkout's text at `path`, or undefined when it is unreadable or binary.
export function readText(root, path) {
  let content;
  try {
    content = readFileSync(resolve(root, path));
  } catch {
    return undefined;
  }
  return content.subarray(0, 8000).includes(0)
    ? undefined
    : content.toString("utf8");
}

// Each patch section is assigned by its own `a/<path> b/<path>` header, since
// a type change (a file replaced by a symlink) yields two sections for one
// name; a header Git had to quote derives no lines.
function patchLines(names, patch) {
  const byHeader = new Map(names.map((path) => [`a/${path} b/${path}`, path]));
  const lines = new Map(names.map((path) => [path, []]));
  for (const section of patch.split(/^diff --git /m).slice(1)) {
    const path = byHeader.get(section.slice(0, section.indexOf("\n")));
    if (path) lines.get(path).push(...hunkLines(section));
  }
  return lines;
}

// Every file the working tree changes against `base`, staged or not, with
// its changed lines; untracked non-ignored files count as added whole.
export function changedAgainst(git, root, base) {
  const diff = ["-c", "core.quotePath=false", "diff", "--no-renames"];
  const names = git(root, [...diff, "--name-only", "-z", base])
    .split("\0")
    .filter(Boolean);
  const lines = patchLines(
    names,
    git(root, [...diff, "--no-ext-diff", "--no-color", "-U0", base]),
  );
  const changes = names.map((path) => ({ path, lines: lines.get(path) }));
  const untracked = git(root, [
    "ls-files",
    "-z",
    "--others",
    "--exclude-standard",
  ])
    .split("\0")
    .filter(Boolean);
  for (const path of untracked) {
    const text = readText(root, path) ?? "";
    const added = text
      .split(/\r?\n/)
      .map((line) => ({ side: "added", text: line }));
    changes.push({ path, lines: added });
  }
  return changes;
}

export function derivedTerms(changes) {
  const terms = [];
  const add = (term, origin) => terms.push({ term, origin, derived: true });
  for (const { path } of changes)
    for (const form of importPathForms(path))
      add(form, `import path of \`${path}\``);
  for (const { path, lines } of changes) {
    if (!PROSE.test(path))
      for (const { text } of lines)
        for (const name of declaredIdentifiers(text))
          add(name, `declared in \`${path}\``);
    const kept = lines
      .filter(({ side }) => side === "added")
      .map(({ text }) => text)
      .join("\n");
    for (const { side, line, text } of lines) {
      if (side !== "removed") continue;
      for (const literal of stringLiterals(text))
        if (literal.length >= MIN_LITERAL && !kept.includes(literal))
          add(literal, `removed from \`${path}\`:${line}`);
    }
  }
  return terms;
}
