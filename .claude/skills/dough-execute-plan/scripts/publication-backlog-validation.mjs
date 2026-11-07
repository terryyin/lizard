// Validate the candidate’s current backlog against its own completion records.
// Publication never repairs either file or scans earlier history for completion.
import { posix, relative, resolve, sep } from "node:path";
import { defaultBacklogPath } from "./owned-suffix-reconciliation.mjs";
import { git } from "./publication-git.mjs";

export async function completedIdentityStop({
  workspace,
  candidate,
  backlogPath: suppliedBacklogPath = defaultBacklogPath,
}) {
  const backlogPath = relative(
    resolve(workspace),
    resolve(workspace, suppliedBacklogPath),
  )
    .split(sep)
    .join("/");
  const present = (
    await git(
      workspace,
      "ls-tree",
      "--name-only",
      "-z",
      candidate,
      "--",
      backlogPath,
    )
  ).stdout.split("\0");
  if (!present.includes(backlogPath)) return null;
  // Execute Plan remains loadable alone when the project has no backlog.
  const [
    { parseBacklog },
    {
      doneRecordPath,
      doneRecordDirectory,
      isDoneRecordFileName,
      requireUncompletedIdentity,
    },
    { BacklogError },
  ] = await Promise.all([
    import("../../dough-product-backlog/scripts/product-backlog-document.mjs"),
    import("../../dough-product-backlog/scripts/product-backlog-done-record.mjs"),
    import("../../dough-product-backlog/scripts/product-backlog-refusal.mjs"),
  ]);
  const directory = posix.dirname(backlogPath);
  const recordDirectory = posix.join(directory, doneRecordDirectory);
  const files = new Set(
    (
      await git(
        workspace,
        "ls-tree",
        "-r",
        "--name-only",
        "-z",
        candidate,
        "--",
        recordDirectory,
      )
    ).stdout
      .split("\0")
      .filter(
        (file) =>
          posix.dirname(file) === recordDirectory &&
          isDoneRecordFileName(posix.basename(file)),
      ),
  );
  if (files.size === 0) return null;
  const source = (await git(workspace, "show", `${candidate}:${backlogPath}`))
    .stdout;
  for (const { identity, list } of parseBacklog(source).entries) {
    const recordPath = posix.join(directory, doneRecordPath(identity));
    if (!files.has(recordPath)) continue;
    const record = (await git(workspace, "show", `${candidate}:${recordPath}`))
      .stdout;
    try {
      requireUncompletedIdentity(identity, record);
    } catch (error) {
      if (!(error instanceof BacklogError)) throw error;
      return {
        status: "completed-identity",
        fields: {
          candidate,
          backlogPath,
          identity,
          recordPath,
          error: `Cannot publish candidate ${candidate}: ${backlogPath} lists identity "${identity}" in ${list}, but ${recordPath} refuses it. ${error.message}`,
        },
      };
    }
  }
  return null;
}
