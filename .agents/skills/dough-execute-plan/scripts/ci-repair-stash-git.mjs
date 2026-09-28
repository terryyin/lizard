// Git reads of the execution checkout and the shared stash stack for the CI
// repair pause, plus the one drop it performs: only an entry found by OID,
// confirmed by the OID Git reports dropping.
import { git } from "./publication-git.mjs";

// Trimmed stdout, or null when the command fails or prints nothing.
export async function gitOutputOrNull(cwd, ...args) {
  try {
    return (await git(cwd, ...args)).stdout.trim() || null;
  } catch {
    return null;
  }
}

// Staged, unstaged, and untracked paths (ignored files are never included).
export async function inventory(checkout) {
  const { stdout } = await git(
    checkout,
    "status",
    "--porcelain=v1",
    "-z",
    "--untracked-files=all",
  );
  const result = { staged: [], unstaged: [], untracked: [] };
  const fields = stdout.split("\0");
  for (let index = 0; index < fields.length; index += 1) {
    const entry = fields[index];
    if (entry.length < 4) continue;
    const [x, y, path] = [entry[0], entry[1], entry.slice(3)];
    if (x === "R" || x === "C") index += 1; // skip the rename source field
    if (x === "?") {
      result.untracked.push(path);
      continue;
    }
    if (x !== " ") result.staged.push(path);
    if (y !== " ") result.unstaged.push(path);
  }
  return result;
}

export function isDirty(paths) {
  return (
    paths.staged.length + paths.unstaged.length + paths.untracked.length > 0
  );
}

export async function stashEntries(checkout) {
  const listed = await gitOutputOrNull(
    checkout,
    "stash",
    "list",
    "--format=%gd%x00%H%x00%gs",
  );
  return (listed ?? "")
    .split("\n")
    .filter(Boolean)
    .map((line) => {
      const [selector, oid, subject] = line.split("\0");
      return { selector, oid, subject };
    });
}

// Git drops by selector, which another writer's push between listing and drop
// would shift, so the OID Git reports dropping ("Dropped stash@{n} (<oid>)")
// confirms whether the dropped entry was the recorded one.
export function dropConfirmation(oid, selector, output) {
  const droppedOid = output.match(/\(([0-9a-f]{40,64})\)/)?.[1] ?? null;
  return droppedOid === oid
    ? { status: "dropped", selector }
    : { status: "mismatch", selector, droppedOid };
}

// The recorded entry wherever other writers' pushes have moved it, or undefined.
export async function stashEntryByOid(checkout, oid) {
  return (await stashEntries(checkout)).find((item) => item.oid === oid);
}

// `missing` when the OID is no longer on the stack; nothing is dropped then.
export async function dropExact(checkout, oid) {
  const entry = await stashEntryByOid(checkout, oid);
  if (!entry) return { status: "missing", selector: null };
  const { stdout, stderr } = await git(
    checkout,
    "stash",
    "drop",
    entry.selector,
  );
  return dropConfirmation(oid, entry.selector, `${stdout}\n${stderr}`);
}
