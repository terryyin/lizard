// A plan's `## Execution complete` record, read independently of its slices
// from the plan's lines and which of them are outside fenced code blocks.
// No filesystem or Node-only imports.

const completionSection = /^## +Execution complete *$/i;
const adviceLine = /^Product advice: *(?<advice>.*?)\s*$/;

// The `## Execution complete` record: absent when the plan has no such
// section, the verbatim advice (continuing until the next `## ` heading) when
// it has a non-empty `Product advice:` entry, and a problem otherwise.
export function readPlanCompletion(lines, unfenced) {
  const start = lines.findIndex(
    (line, index) => unfenced[index] && completionSection.test(line),
  );
  if (start === -1) {
    return undefined;
  }
  const next = lines.findIndex(
    (line, index) => index > start && unfenced[index] && /^## /.test(line),
  );
  const end = next === -1 ? lines.length : next;
  const entryIndex = lines.findIndex(
    (line, index) =>
      index > start && index < end && unfenced[index] && adviceLine.test(line),
  );
  if (entryIndex !== -1) {
    const advice = [
      adviceLine.exec(lines[entryIndex]).groups.advice,
      ...lines.slice(entryIndex + 1, end).map((line) => line.trimEnd()),
    ]
      .join("\n")
      .trim();
    if (advice !== "") {
      return { advice };
    }
  }
  return {
    problem:
      "This plan’s “## Execution complete” record has no readable “Product advice:” entry.",
  };
}
