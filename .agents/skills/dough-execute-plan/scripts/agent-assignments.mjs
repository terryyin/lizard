// Agent assignments as Git records them: the profiles held beside the backlog
// at a revision, the rotation's next free name, the allocation (the commit
// that added a profile), and the profile a commit added. Execution Takes and
// preparation announcements share this reading; neither owns it.
import { basename, dirname, join } from "node:path";
import {
  agentIdentity,
  agentProfileDirectory,
  parseAgentProfile,
  parseAgentProfileFile,
  profileAgentName,
  selectAgentName,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { git } from "./publication-git.mjs";
import { stopped } from "./workspace-publication-ownership.mjs";

// Every path a Git command lists under the profile directory beside the
// backlog, in listed order, whether or not it names a rotation agent.
async function listedPaths(cwd, backlogPath, ...args) {
  const directory = join(dirname(backlogPath), agentProfileDirectory);
  const { stdout } = await git(cwd, ...args, "--", `${directory}/`);
  return stdout.split("\n").filter(Boolean);
}

// Agent profiles (path and agent name) a Git command lists under the profile
// directory beside the backlog, in listed order.
async function listedProfiles(cwd, backlogPath, ...args) {
  const paths = await listedPaths(cwd, backlogPath, ...args);
  return paths
    .map((path) => ({ path, name: profileAgentName(basename(path)) }))
    .filter(({ name }) => name);
}

async function listedAgentNames(cwd, backlogPath, ...args) {
  const profiles = await listedProfiles(cwd, backlogPath, ...args);
  return profiles.map(({ name }) => name);
}

// Agent names whose profiles exist beside the backlog at `rev`.
function heldAgentNames(cwd, rev, backlogPath) {
  return listedAgentNames(cwd, backlogPath, "ls-tree", "--name-only", rev);
}

// The agent name whose profile was most recently added beside the backlog in
// `rev`'s first-parent history, released or not; undefined when none ever was.
async function mostRecentAgentName(cwd, rev, backlogPath) {
  const added = await listedAgentNames(
    cwd,
    backlogPath,
    "log",
    "--first-parent",
    "--diff-filter=A",
    "--name-only",
    "--format=",
    rev,
  );
  return added[0];
}

// The rotation's next name at `rev`, or undefined when every name is held.
// Every profile file occupies its name, whatever activity it records.
export async function nextAgentName(cwd, rev, backlogPath) {
  const [mostRecent, held] = await Promise.all([
    mostRecentAgentName(cwd, rev, backlogPath),
    heldAgentNames(cwd, rev, backlogPath),
  ]);
  return { name: selectAgentName(mostRecent, held), held };
}

// The allocation a published profile records: the commit that most recently
// added the file at `path` in `rev`'s history. A later reuse of the same name
// is a different allocation even when its text is identical. Undefined when
// `rev` never added it.
export async function profileAllocation(cwd, rev, path) {
  const { stdout } = await git(
    cwd,
    "log",
    "-1",
    "--diff-filter=A",
    "--format=%H",
    rev,
    "--",
    path,
  );
  return stdout.trim() || undefined;
}

// Every file in the profile directory at `rev`: each held name's recorded
// work and allocation, and any file that is not a readable profile. It
// diagnoses a full rotation and names who holds a story. Nothing here is
// released or reclaimed; age and absence of a local process are not reasons
// to free a name.
export async function occupiedAssignments(cwd, rev, backlogPath) {
  const paths = await listedPaths(
    cwd,
    backlogPath,
    "ls-tree",
    "--name-only",
    rev,
  );
  return Promise.all(
    paths.map(async (path) => {
      const allocation = await profileAllocation(cwd, rev, path);
      const text = (await git(cwd, "cat-file", "-p", `${rev}:${path}`)).stdout;
      const read = parseAgentProfileFile(basename(path), text);
      if (!read.ok) return { path, allocation, unrecognized: read.error };
      const { name, ...work } = read.profile;
      return { path, agent: agentIdentity(name).agent, ...work, allocation };
    }),
  );
}

// The stop for a full rotation at `rev`, carrying `extra` and every
// occupied assignment for diagnosis.
export async function agentUnavailable(cwd, rev, backlogPath, extra = {}) {
  return stopped("agent-unavailable", {
    ...extra,
    occupied: await occupiedAssignments(cwd, rev, backlogPath),
    error: "every agent name is held on remote trunk",
  });
}

// The rotation's next name at `rev` as read in the checkout `cwd`, carrying
// what the agent reported about itself; when every name is held, a stop
// carrying `stopFields`.
export async function selectAgent(
  { cwd, host, model },
  rev,
  backlogPath,
  stopFields,
) {
  const { name } = await nextAgentName(cwd, rev, backlogPath);
  if (!name) return agentUnavailable(cwd, rev, backlogPath, stopFields);
  return {
    ok: true,
    agent: {
      name,
      ...(host === undefined ? {} : { host }),
      ...(model === undefined ? {} : { model }),
    },
  };
}

// The first readable profile commit `sha` added beside the backlog that
// `accepts(profile, name)` recognizes, where `name` is the agent its file
// name holds: `{ path, name, profile }`, or undefined. This is how a claim or
// preparation announcement names its own assignment.
export async function addedProfile(cwd, sha, backlogPath, accepts) {
  const added = await listedProfiles(
    cwd,
    backlogPath,
    "diff-tree",
    "--no-commit-id",
    "-r",
    "--name-only",
    "--diff-filter=A",
    sha,
  );
  for (const { path, name } of added) {
    const text = (await git(cwd, "show", `${sha}:${path}`)).stdout;
    const read = parseAgentProfile(text);
    if (read.ok && accepts(read.profile, name))
      return { path, name, profile: read.profile };
  }
  return undefined;
}
