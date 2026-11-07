// Reads the recorded Goal from a story's canonical home. Missing Goal is
// Not recorded — never inferred from free-form Status prose. Shares the home
// region with story-state without filesystem access.

import { readHome } from "./product-backlog-home-reader.mjs";

const goalLine = /^\*\*Goal:\*\* *(?<goal>.*?)\s*$/;
const nextField = /^\*\*[^*]+:\*\*/;
const stopLine = /^(?:### |## |<a id=|```)/;

export function readStoryPurpose(source, href) {
  const home = readHome(source, href);
  const { lines } = home.document;
  const { start, end } = home.region;
  for (let index = start; index < end; index += 1) {
    const match = goalLine.exec(lines[index]);
    if (!match) {
      continue;
    }
    const parts = [];
    const first = match.groups.goal.trim();
    if (first !== "") {
      parts.push(first);
    }
    for (let follow = index + 1; follow < end; follow += 1) {
      const line = lines[follow];
      if (line.trim() === "") {
        if (parts.length === 0) {
          continue;
        }
        break;
      }
      if (nextField.test(line) || stopLine.test(line)) {
        break;
      }
      parts.push(line.trimEnd());
    }
    const purpose = parts.join("\n").trim();
    if (purpose === "") {
      return { status: "not-recorded" };
    }
    return { status: "recorded", purpose };
  }
  return { status: "not-recorded" };
}
