// The text shape of every Markdown file this tool edits: the backlog itself and
// the canonical homes it links to. Splitting a file into lines here, and
// joining it back here, is what lets every untouched line survive a change byte
// for byte, whatever line ending and final-newline convention the file uses.

export function splitSource(source) {
  const newline = source.includes("\r\n") ? "\r\n" : "\n";
  const hasFinalNewline = source.endsWith(newline);
  const lines = source.split(/\r?\n/);
  if (hasFinalNewline) {
    lines.pop();
  }
  return { lines, newline, hasFinalNewline };
}

export function joinSource({ lines, newline, hasFinalNewline }) {
  const body = lines.join(newline);
  return hasFinalNewline ? `${body}${newline}` : body;
}
