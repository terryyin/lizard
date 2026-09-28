// The commit an assigned agent makes for its own work in its owned workspace:
// an ordinary `git commit` of the staged content, so the checkout's hooks run,
// whose message credits the configured developer once beside any co-authors
// it already names. The agent is the Git author the workspace was configured
// with when its work was Taken or announced; a checkout without one is not an
// agent's workspace, and nothing is committed there.
import { readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";
import { exec, git, revParse } from "./publication-git.mjs";
import {
  creditDeveloper,
  DeveloperIdentityRefused,
  workspaceAgent,
} from "./workspace-agent-authorship.mjs";

const usage =
  "usage: agent-commit.mjs [-C <workspace>] (-m <message>... | -F <file|->) [--amend]\n" +
  "       agent-commit.mjs [-C <workspace>] --amend   (keeps HEAD's message)";

const refused = (status, error) => ({ ok: false, status, error });

// Commits the staged content of `workspace` with `message`, or amends HEAD
// with it (HEAD's own message when none is given). Returns the receipt; a
// refusal commits nothing.
export async function agentCommit(workspace, { message, amend = false }) {
  const identity = await workspaceAgent(workspace);
  if (!identity)
    return refused(
      "no-workspace-agent",
      "this checkout's Git config names no assigned agent as author, so it is not an agent's owned workspace",
    );
  const text =
    message ?? (await git(workspace, "log", "-1", "--format=%B")).stdout;
  let credited;
  try {
    credited = await creditDeveloper(workspace, text, identity);
  } catch (error) {
    if (!(error instanceof DeveloperIdentityRefused)) throw error;
    return refused("developer-identity-refused", error.message);
  }
  const committing = exec(
    "git",
    ["commit", "--quiet", ...(amend ? ["--amend"] : []), "-F", "-"],
    { cwd: workspace },
  );
  committing.child.stdin.end(credited);
  try {
    await committing;
  } catch (error) {
    const output = `${error.stdout ?? ""}${error.stderr ?? ""}`.trim();
    return refused("commit-failed", output || error.message);
  }
  return {
    ok: true,
    status: amend ? "amended" : "committed",
    agent: identity.agent,
    sha: await revParse(workspace, "HEAD"),
  };
}

async function readStdin() {
  const chunks = [];
  for await (const chunk of process.stdin) chunks.push(chunk);
  return Buffer.concat(chunks).toString("utf8");
}

// `git commit`'s own spelling of the options this entry point accepts.
async function requestOf(argv) {
  let workspace = process.cwd(),
    file,
    amend = false;
  const paragraphs = [];
  for (let index = 0; index < argv.length; index += 1) {
    const option = argv[index];
    if (option === "--amend") {
      amend = true;
      continue;
    }
    const value = argv[++index];
    if (value === undefined || !["-C", "-m", "-F"].includes(option))
      throw new Error(`invalid argument ${option}\n${usage}`);
    if (option === "-C") workspace = resolve(value);
    else if (option === "-m") paragraphs.push(value);
    else file = value;
  }
  if (file !== undefined && paragraphs.length > 0)
    throw new Error(`use either -m or -F\n${usage}`);
  if (file === undefined && paragraphs.length === 0 && !amend)
    throw new Error(`a message is required\n${usage}`);
  let message;
  if (paragraphs.length > 0) message = `${paragraphs.join("\n\n")}\n`;
  else if (file === "-") message = await readStdin();
  else if (file !== undefined)
    message = await readFile(resolve(workspace, file), "utf8");
  return { workspace, message, amend };
}

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    const { workspace, ...commit } = await requestOf(process.argv.slice(2));
    const result = await agentCommit(workspace, commit);
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (!result.ok) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 2;
  }
}
