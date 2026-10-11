// Whether and how an owned workspace's ordinary commits name its agent as
// author through per-worktree Git config, and the developer credit every
// agent-enabled commit carries as a co-author trailer.
import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import {
  agentIdentity,
  agentNameOf,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { exec, git } from "./publication-git.mjs";

// "configured" when the workspace's ordinary commits can be authored through
// per-worktree config. A bare repository's worktrees rely on `core.bare` or
// `core.worktree` in the shared config; enabling `extensions.worktreeConfig`
// there breaks every checkout, so such a workspace is "not-configured".
export async function workspaceAuthorship(workspace) {
  const read = async (...args) => (await git(workspace, ...args)).stdout.trim();
  const common = await read(
    "rev-parse",
    "--path-format=absolute",
    "--git-common-dir",
  );
  const shared = (...args) =>
    read("config", "--file", join(common, "config"), ...args);
  const bare = await shared("--type=bool", "--default=false", "core.bare");
  const worktree = await shared("--default=", "core.worktree");
  return bare === "true" || worktree !== "" ? "not-configured" : "configured";
}

// The agent authors every ordinary commit in its owned workspace; the
// configured Git user stays the committer, and other checkouts keep their
// usual author. Leaves a bare repository's shared config untouched. Returns
// the workspace's authorship, which configuring does not change.
export async function configureAgentAuthorship(workspace, { agent, email }) {
  const authorship = await workspaceAuthorship(workspace);
  if (authorship !== "configured") return authorship;
  await git(workspace, "config", "extensions.worktreeConfig", "true");
  await git(workspace, "config", "--worktree", "author.name", agent);
  await git(workspace, "config", "--worktree", "author.email", email);
  return authorship;
}

// The rotation agent this workspace's own Git config names as author, or
// undefined when it names none (another checkout, or a workspace whose
// authorship could not be configured). Reads what configureAgentAuthorship
// writes.
export async function workspaceAgent(workspace) {
  const read = async (key) => {
    try {
      return (
        await git(workspace, "config", "--worktree", "--get", key)
      ).stdout.trim();
    } catch {
      return "";
    }
  };
  const name = agentNameOf(await read("author.name"));
  if (!name) return undefined;
  const identity = agentIdentity(name);
  return (await read("author.email")) === identity.email ? identity : undefined;
}

// Refuses an agent-enabled commit whose developer credit would be missing or
// misleading. Its message names the committer problem for the developer.
export class DeveloperIdentityRefused extends Error {}

const identLine = /^(.*) <([^<>]*)> \d+ [+-]\d{4}$/;
const emailShape = /^[^\s@<>]+@[^\s@<>]+$/;

// The developer an agent-enabled commit in `checkout` credits: the effective
// committer Git would record there, configured rather than guessed from the
// host, and distinct from the authoring `agent` ({ agent, email }). Returns
// that `person` ("Name <email>") and its email `address`, or throws
// DeveloperIdentityRefused.
async function developerCoAuthor(checkout, { agent, email }) {
  let ident;
  try {
    ident = (
      await git(
        checkout,
        "-c",
        "user.useConfigOnly=true",
        "var",
        "GIT_COMMITTER_IDENT",
      )
    ).stdout.trim();
  } catch (error) {
    const reason = (error.stderr || error.message).trim().split("\n").at(-1);
    throw new DeveloperIdentityRefused(
      `Git has no usable committer identity for developer credit: ${reason}`,
    );
  }
  const [, name = "", address = ""] = ident.match(identLine) ?? [];
  const person = `${name} <${address}>`;
  if (name.trim() === "" || !emailShape.test(address))
    throw new DeveloperIdentityRefused(
      `Git committer identity is malformed for developer credit: ${person}`,
    );
  if (
    name.toLowerCase() === agent.toLowerCase() ||
    address.toLowerCase() === email.toLowerCase()
  )
    throw new DeveloperIdentityRefused(
      `Git committer identity is the agent's own, so no developer can be credited: ${person}`,
    );
  return { person, address };
}

// Runs `git interpret-trailers` in `checkout` over `message`.
async function interpretTrailers(checkout, message, ...args) {
  const running = exec("git", ["interpret-trailers", ...args], {
    cwd: checkout,
  });
  running.child.stdin.end(message);
  return (await running).stdout;
}

// Whether `message` already credits the person at `address` as a co-author,
// however it spells the trailer key, the email's case, or the display name.
async function coAuthorCredited(checkout, message, address) {
  const parsed = await interpretTrailers(checkout, message, "--parse");
  return parsed.split("\n").some((line) => {
    const [, key = "", credited = ""] =
      line.match(/^([^:]*):.*<([^<>]*)>\s*$/) ?? [];
    return (
      key.trim().toLowerCase() === "co-authored-by" &&
      credited.toLowerCase() === address.toLowerCase()
    );
  });
}

// `message` with the developer committing in `checkout` credited once as a
// `Co-authored-by` trailer beside any co-authors it already names; a message
// that already credits that developer's email, as an amended or replayed one
// does, is returned as it is. Throws DeveloperIdentityRefused before any
// commit when that developer is unusable beside the authoring agent
// `identity` ({ agent, email }).
export async function creditDeveloper(checkout, message, identity) {
  const { person, address } = await developerCoAuthor(checkout, identity);
  if (await coAuthorCredited(checkout, message, address)) return message;
  return interpretTrailers(
    checkout,
    message,
    "--if-exists",
    "add",
    "--trailer",
    `Co-authored-by: ${person}`,
  );
}

// `message` credited like any other agent commit when `workspace` is an
// agent's owned workspace, or as it is in a checkout that names no agent.
// Throws DeveloperIdentityRefused when that developer is unusable.
export async function creditInWorkspace(workspace, message) {
  const identity = await workspaceAgent(workspace);
  return identity ? creditDeveloper(workspace, message, identity) : message;
}

// Credits the developer on the merge in progress in `workspace` when it is an
// agent's owned workspace: Git's prepared merge message gains the developer
// credit, so the ordinary `git commit` that concludes the merge is credited
// like any other agent commit. A checkout that names no agent keeps Git's
// message as it is. Throws DeveloperIdentityRefused, leaving the merge
// uncommitted, when that developer is unusable.
export async function creditMergeInProgress(workspace) {
  const path = (
    await git(
      workspace,
      "rev-parse",
      "--path-format=absolute",
      "--git-path",
      "MERGE_MSG",
    )
  ).stdout.trim();
  const message = await readFile(path, "utf8");
  await writeFile(path, await creditInWorkspace(workspace, message));
}
