// The consumer-trace record: each located file's kind, and the Markdown an
// agent fills with a proof disposition for every listed consumer.

const CONSUMER_KINDS = [
  { kind: "production", heading: "Production" },
  { kind: "test", heading: "Test" },
  { kind: "stand-in", heading: "Test support or stand-in" },
  { kind: "journey", heading: "Journey or feature" },
  { kind: "guidance", heading: "Guidance reader" },
];

const JOURNEY =
  /(\.feature$)|((^|\/)(step_definitions|pageObjects)(\/|$))|((^|[/._-])e2e([/._-]|$))/i;
const TEST_NAME = /(\.(test|spec)\.)|([_-](test|spec)\.[^/]+$)/i;
const STAND_IN =
  /(^|[/._-])(support|fixtures?|helpers?|stubs?|fakes?|mocks?)([/._-]|$)/i;
const TEST_DIRECTORY = /(^|\/)(tests?|[^/]*_test)\//i;

// A hint for inspection, one kind per file. The first rule that holds wins:
// journey, test file name, stand-in, test directory, Markdown, production.
// So `tests/support/x.ts` is a stand-in, `support/x.test.mjs` is a test and
// `e2e_test/step_definitions/x.ts` is a journey.
export function consumerKind(path) {
  if (JOURNEY.test(path)) return "journey";
  if (TEST_NAME.test(path)) return "test";
  if (STAND_IN.test(path)) return "stand-in";
  if (TEST_DIRECTORY.test(path)) return "test";
  if (/\.md$/i.test(path)) return "guidance";
  return "production";
}

const cell = (text) => String(text).replace(/\|/g, "\\|");

function code(text) {
  const value = cell(text);
  if (!value) return "";
  return value.includes("`") ? `\`\` ${value} \`\`` : `\`${value}\``;
}

function shorten(text, limit = 160) {
  return text.length > limit ? `${text.slice(0, limit - 1)}…` : text;
}

export function renderConsumerTrace(record) {
  const out = ["# Consumer trace", "", "## Effect terms searched", ""];
  out.push("| Term | Origin |", "| --- | --- |");
  for (const { term, origin, broad } of record.terms) {
    const note = broad
      ? `; in ${broad} files, too broad to list consumers by itself; supply it as a \`--term\` to list them`
      : "";
    out.push(`| ${code(term)} | ${cell(origin)}${cell(note)} |`);
  }
  out.push("", "## Scope", "");
  out.push(
    `Whole checkout at \`${record.revision}\`, including uncommitted and untracked files: ${record.fileCount} tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.`,
  );
  out.push(
    "",
    record.changed.length
      ? `Changed files not searched: ${record.changed.map(code).join(", ")}.`
      : "Changed files not searched: none named.",
  );
  out.push("", "## Located consumers", "");
  out.push(
    "Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.",
  );
  for (const { kind, heading } of CONSUMER_KINDS) {
    out.push("", `### ${heading}`, "");
    const found = record.consumers.filter((item) => item.kind === kind);
    if (!found.length) {
      out.push("none found");
      continue;
    }
    out.push(
      "| File | Terms matched | First matching line | Disposition |",
      "| --- | --- | --- | --- |",
    );
    for (const item of found)
      out.push(
        `| ${code(item.path)} | ${item.terms.map(code).join(", ")} | ${item.line}: ${code(shorten(item.text))} | |`,
      );
  }
  return `${out.join("\n")}\n`;
}
