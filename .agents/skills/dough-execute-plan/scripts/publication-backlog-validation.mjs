// Validate the candidate’s current backlog against its own completion records,
// and hold a candidate holding an agent profile among those records.
// Publication never repairs either file or scans earlier history for completion.
import { posix, relative, resolve, sep } from "node:path";
import { defaultBacklogPath } from "./owned-suffix-reconciliation.mjs";
import { git } from "./publication-git.mjs";

export async function doneRecordsStop({
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
    {
      describeMisplacedAgentProfiles,
      hasAgentProfileFileName,
      misplacedAgentProfiles,
    },
    { BacklogError },
  ] = await Promise.all([
    import("../../dough-product-backlog/scripts/product-backlog-document.mjs"),
    import("../../dough-product-backlog/scripts/product-backlog-done-record.mjs"),
    import("../../dough-product-backlog/scripts/product-backlog-misplaced-agent-profile.mjs"),
    import("../../dough-product-backlog/scripts/product-backlog-refusal.mjs"),
  ]);
  const directory = posix.dirname(backlogPath);
  const recordDirectory = posix.join(directory, doneRecordDirectory);
  const listed = (
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
    .filter((file) => posix.dirname(file) === recordDirectory);
  const profileNamed = await Promise.all(
    listed
      .filter((file) => hasAgentProfileFileName(posix.basename(file)))
      .map(async (file) => ({
        fileName: posix.basename(file),
        text: (await git(workspace, "show", `${candidate}:${file}`)).stdout,
      })),
  );
  const misplaced = misplacedAgentProfiles(directory, profileNamed);
  if (misplaced.length > 0) {
    return {
      status: "misplaced-agent-profile",
      fields: {
        candidate,
        backlogPath,
        misplaced,
        error: `Cannot publish candidate ${candidate}: it holds an agent profile among the done records beside ${backlogPath}. Move each profile back to the path named below and commit that, then publish again.\n${describeMisplacedAgentProfiles(misplaced)}`,
      },
    };
  }
  const files = new Set(
    listed.filter((file) => isDoneRecordFileName(posix.basename(file))),
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
