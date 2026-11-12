// Effect terms derived from the change itself: the files a working tree
// changes against a base revision, the identifiers its changed lines declare,
// each changed module's import-path forms and the literals its removed lines
// held. Derivation is lexical; behavior the diff cannot carry stays supplied.
import { basename, dirname, extname } from "node:path";

export {
  changedAgainst,
  readText,
  repositoryChangePaths,
} from "./git-changes.mjs";

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
