// Proves that a retained preparation stopped before committing its announcement,
// then removes only its isolated canonical staged profile so startup can refresh.
import { lstatSync, readFileSync, unlinkSync } from "node:fs";
import { basename, join } from "node:path";
import {
  parseAgentProfileFile,
  renderAgentProfile,
} from "../../dough-product-backlog/scripts/product-backlog-agent-profile.mjs";
import { occupiedAssignments } from "../../dough-execute-plan/scripts/agent-assignments.mjs";
import { ongoingOperation } from "../../dough-execute-plan/scripts/maintain-default-checkout.mjs";
import {
  git,
  revParse,
} from "../../dough-execute-plan/scripts/publication-git.mjs";
import {
  backlogPath,
  isAncestor,
} from "../../dough-execute-plan/scripts/workspace-publication-ownership.mjs";
import { profilePathOf } from "./preparation-assignment-ownership.mjs";

// Explicit recovery proves an interrupted start never committed an announcement.
// A missing receipt alone cannot authorize another allocation.
export async function recoverUnannounced(request, ref, recorded) {
  const { workspace, identity } = request;
  if (
    !request.recoverUnannounced ||
    recorded !== undefined ||
    request.expectedAgent !== undefined ||
    request.expectedAllocation !== undefined
  )
    return false;
  if (await ongoingOperation(workspace)) return false;
  const head = await revParse(workspace, "HEAD");
  if (!(await isAncestor(workspace, head, ref))) return false;
  const history = await git(
    workspace,
    "log",
    "--format=%H",
    "--fixed-strings",
    `--grep=Preparation-Identity: ${identity}`,
    "HEAD",
  );
  if (history.stdout !== "") return false;
  const occupied = await occupiedAssignments(workspace, ref, backlogPath);
  if (occupied.some((assignment) => assignment.identity === identity))
    return false;
  const status = (await git(workspace, "status", "--porcelain", "-z")).stdout;
  if (status === "") return true;
  const match = /^A {2}([^\0]+)\0$/.exec(status);
  if (!match) return false;
  const path = match[1];
  const file = join(workspace, path);
  if (!lstatSync(file).isFile()) return false;
  const bytes = readFileSync(file, "utf8");
  const parsed = parseAgentProfileFile(basename(path), bytes);
  if (!parsed.ok) return false;
  const profile = parsed.profile;
  if (
    path !== profilePathOf(profile.name) ||
    profile.activity !== "preparation" ||
    profile.identity !== identity ||
    profile.host !== request.host ||
    profile.model !== request.model ||
    bytes !== renderAgentProfile(profile)
  )
    return false;
  const staged = (await git(workspace, "show", `:${path}`)).stdout;
  if (staged !== bytes) return false;
  // Remove only this isolated precommit artifact. Refresh restores any later
  // allocation of its name from trunk before choosing a currently free agent.
  await git(workspace, "rm", "--cached", "--quiet", "--", path);
  unlinkSync(file);
  return true;
}
