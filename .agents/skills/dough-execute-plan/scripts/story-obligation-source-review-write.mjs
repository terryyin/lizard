import { rename, unlink, writeFile } from "node:fs/promises";
import { readSection, unfencedLines } from "./story-obligation-reader.mjs";
import { tableCells } from "./story-obligation-source-review.mjs";

// Renders and rewrites Commitments rows and Story obligations entries in the
// shapes existing readers already accept. Atomic multi-file replace lives here
// so the recording path can refuse partial writes.

export function escapeCell(text) {
  return String(text).replace(/\|/g, "\\|");
}

export function renderCommitmentRow({ id, text, slice, observed, obligation }) {
  const allocation =
    slice === undefined || slice === null ? "" : `slice ${slice}`;
  let state = "";
  if (observed !== undefined && observed !== null)
    state = `observed: ${observed}`;
  else if (obligation) state = obligation;
  return `| ${escapeCell(id)} | ${escapeCell(text)} | ${escapeCell(allocation)} | ${escapeCell(state)} |`;
}

export function renderObligationEntry(entry) {
  const lines = [
    `### ${entry.id}. ${entry.title}`,
    `Reported: slice ${entry.reportedSlice} — "${entry.reported}"`,
    `Story clause: "${entry.storyClause}"`,
    `Disposition: ${entry.disposition}`,
  ];
  if (entry.commitments?.length)
    lines.push(`Commitment: ${entry.commitments.join(", ")}`);
  return lines.join("\n");
}

export function replaceCommitmentLine(reviewSource, commitment, next) {
  const lines = reviewSource.split(/\r?\n/);
  const section = readSection(
    unfencedLines(reviewSource),
    /^## +Commitments\s*$/,
  );
  if (section.count !== 1) return undefined;
  const start = lines.findIndex((line) => /^## +Commitments\s*$/.test(line));
  if (start === -1) return undefined;
  let replaced = false;
  for (let index = start + 1; index < lines.length; index += 1) {
    if (/^## /.test(lines[index])) break;
    if (!/^\s*\|/.test(lines[index])) continue;
    if (/^\s*\|[\s:|-]+$/.test(lines[index])) continue;
    if (/^\s*\|\s*Id\s*\|/i.test(lines[index])) continue;
    const cells = tableCells(lines[index]);
    if (cells[0] !== commitment) continue;
    lines[index] = next;
    replaced = true;
    break;
  }
  return replaced ? lines.join("\n") : undefined;
}

export function upsertObligationEntry(planSource, entry) {
  const lines = planSource.split(/\r?\n/);
  const rendered = renderObligationEntry(entry);
  const heading = new RegExp(
    `^### +${entry.id.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\. `,
  );
  const sectionStart = lines.findIndex((line) =>
    /^## +Story obligations\s*$/.test(line),
  );
  if (sectionStart === -1) {
    const insertAt = lines.length;
    const block = ["", "## Story obligations", "", rendered, ""];
    lines.splice(insertAt, 0, ...block);
    return lines.join("\n");
  }
  let sectionEnd = lines.length;
  for (let index = sectionStart + 1; index < lines.length; index += 1) {
    if (/^## /.test(lines[index])) {
      sectionEnd = index;
      break;
    }
  }
  let entryStart = -1;
  for (let index = sectionStart + 1; index < sectionEnd; index += 1) {
    if (heading.test(lines[index])) {
      entryStart = index;
      break;
    }
  }
  if (entryStart === -1) {
    const prefix = lines
      .slice(sectionStart + 1, sectionEnd)
      .some((line) => line.trim())
      ? ["", rendered]
      : [rendered, ""];
    lines.splice(sectionEnd, 0, ...prefix);
    return lines.join("\n");
  }
  let entryEnd = sectionEnd;
  for (let index = entryStart + 1; index < sectionEnd; index += 1) {
    if (/^### /.test(lines[index])) {
      entryEnd = index;
      break;
    }
  }
  while (
    entryEnd > entryStart + 1 &&
    !lines[entryEnd - 1].trim() &&
    (entryEnd === sectionEnd || !lines[entryEnd]?.trim())
  )
    entryEnd -= 1;
  const replacement = rendered.split("\n");
  lines.splice(entryStart, entryEnd - entryStart, ...replacement);
  return lines.join("\n");
}

export async function replaceFiles(updates) {
  const staged = [];
  try {
    for (const update of updates) {
      const temporary = `${update.path}.tmp-${process.pid}-${staged.length}`;
      await writeFile(temporary, update.contents, "utf8");
      staged.push({ ...update, temporary });
    }
    const renamed = [];
    try {
      for (const item of staged) {
        await rename(item.temporary, item.path);
        renamed.push(item);
      }
    } catch (error) {
      for (const item of renamed) {
        await writeFile(item.path, item.previous, "utf8");
      }
      throw error;
    }
  } catch (error) {
    for (const item of staged) {
      await unlink(item.temporary).catch(() => {});
    }
    throw error;
  }
}
