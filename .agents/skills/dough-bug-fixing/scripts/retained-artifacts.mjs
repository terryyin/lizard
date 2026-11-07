// Git-free mechanics for bug remaining-work retention: removes only the
// caller's named disposable paths inside the owned workspace and leaves the
// durable record as a local draft. It never commits or pushes; an explicit
// keep lands the workspace through Dough Land.
import { rmSync } from "node:fs";
import { isAbsolute, relative, resolve } from "node:path";

function insideWorkspace(workspace, candidate) {
  const root = resolve(workspace);
  const target = resolve(workspace, candidate);
  const rel = relative(root, target);
  return rel !== "" && !rel.startsWith("..") && !isAbsolute(rel);
}

function removeNamedDisposablePaths(workspace, disposablePaths, durablePath) {
  const durable = resolve(workspace, durablePath);
  for (const disposablePath of disposablePaths) {
    if (!insideWorkspace(workspace, disposablePath)) continue;
    const target = resolve(workspace, disposablePath);
    if (target === durable) continue;
    rmSync(target, { force: true });
  }
}

export async function retainBugTriageArtifacts({
  workspace,
  disposablePaths,
  durablePath,
}) {
  removeNamedDisposablePaths(workspace, disposablePaths, durablePath);
  return {
    disposition: "pending",
    detail:
      "pending disposition: local draft is not explicitly retained and stays in the owned workspace",
    published: false,
  };
}
