// Locates and encodes one versioned story-state fence inside canonical-home
// text. Reading and writing the fence shape lives here so preparation and
// later assessment share one block encoding without a second Markdown status
// grammar and without importing filesystem access.

import { BacklogError } from "./product-backlog-refusal.mjs";

export const storyStateFence = "json dough-story-state";

const openFenceLine = `\`\`\`${storyStateFence}`;

export function storyStateBlockLines(payload) {
  return [openFenceLine, JSON.stringify(payload), "```"];
}

export function findStoryStateBlocks(lines, region) {
  const found = [];
  let index = region.start;
  while (index < region.end) {
    if (lines[index] !== openFenceLine) {
      index += 1;
      continue;
    }
    const open = index;
    let close = -1;
    for (let cursor = open + 1; cursor < region.end; cursor += 1) {
      if (lines[cursor] === "```") {
        close = cursor;
        break;
      }
      if (lines[cursor] === openFenceLine) {
        break;
      }
    }
    if (close === -1) {
      throw new BacklogError(
        `Story-state fence opened at line ${open + 1} is not closed inside ` +
          `the selected home; repair it by hand before recording or reading.`,
      );
    }
    found.push({
      open,
      close,
      body: lines.slice(open + 1, close).join("\n"),
    });
    index = close + 1;
  }
  return found;
}

export function parseStoryStatePayload(body, location) {
  let value;
  try {
    value = JSON.parse(body);
  } catch {
    throw new BacklogError(
      `Story-state block at ${location} is not valid JSON; repair it by hand ` +
        `before recording or reading.`,
    );
  }
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    throw new BacklogError(
      `Story-state block at ${location} must be one JSON object.`,
    );
  }
  return value;
}

export function storyStateSourceLocation(home, open, close) {
  return {
    path: home.relative,
    href: home.href,
    startLine: open + 1,
    endLine: close + 1,
  };
}
