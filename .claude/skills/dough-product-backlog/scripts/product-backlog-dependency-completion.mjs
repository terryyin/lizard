// Validate recoverable outcome and accepted integration as distinct facts.
import { dirname, relative, resolve, sep } from "node:path";
import {
  collectingGitDiagnostics,
  git,
  gitOutcome,
  repositoryRoot,
} from "./product-backlog-git-repository.mjs";
import { parseBacklog } from "./product-backlog-document.mjs";
import { namedIdentity, readHome } from "./product-backlog-home-reader.mjs";
import { splitHref } from "./product-backlog-identity.mjs";
import { readPlanSlices } from "./product-backlog-plan-reader.mjs";
import { BacklogError, requireField } from "./product-backlog-refusal.mjs";
import { pathFor } from "./product-backlog-story-dependencies-command.mjs";
import { declaredPlanTarget } from "./product-backlog-story-state-home.mjs";
import { readStoryState } from "./product-backlog-story-state.mjs";

function revision(value, field) {
  requireField(value, field);
  if (!/^[0-9a-f]{40,64}$/.test(value))
    throw new BacklogError(`${field} must be a full Git revision.`);
  return value;
}

export async function supplierCompletion(file, dependency, values) {
  const captured = await collectingGitDiagnostics(() => {
    const root = repositoryRoot(dirname(file));
    const evidence = revision(
      dependency.resolution.revision,
      "resolution.revision",
    );
    const accepted = revision(values["accepted-revision"], "accepted-revision");
    requireField(values.remote, "remote");
    requireField(values.target, "target");
    const target = values.target.startsWith("refs/heads/")
      ? values.target
      : `refs/heads/${values.target}`;
    git(["check-ref-format", target], root);
    git(["fetch", "--quiet", values.remote, target], root);
    for (const [before, after] of [
      [accepted, "FETCH_HEAD"],
      [evidence, accepted],
    ]) {
      if (
        gitOutcome(["merge-base", "--is-ancestor", before, after], root)
          .code !== 0
      )
        throw new BacklogError(
          "Supplier outcome and accepted revision must be contained in the authorized integration target. Nothing was written.",
        );
    }
    const supplierPath = pathFor(file, dependency.supplier.href);
    const path = relative(root, supplierPath).split(sep).join("/");
    const { anchor } = splitHref(dependency.supplier.href);
    const locator = anchor ? `${path}#${anchor}` : path;
    if (dependency.resolution.path !== locator)
      throw new BacklogError(
        "Resolution path must name the recoverable supplier canonical home.",
      );
    const source = git(["show", `${evidence}:${path}`], root);
    if (
      namedIdentity(readHome(source, dependency.supplier.href)) !==
      dependency.supplier.identity
    )
      throw new BacklogError(
        "Recovered supplier home does not name the selected identity.",
      );
    const preparation = readStoryState(source, dependency.supplier.href);
    if (preparation.status === "uninterpretable")
      throw new BacklogError(
        "Supplier preparation is unreadable; establish its completion context before resolving.",
      );
    const declared = declaredPlanTarget(
      dirname(file),
      dependency.supplier.href,
      preparation.approach?.kind,
      preparation.approach?.plan,
    );
    if (declared && values.plan && declared !== values.plan)
      throw new BacklogError(
        "Supplied plan disagrees with the supplier canonical preparation.",
      );
    const backlogPath = relative(root, file).split(sep).join("/");
    const selected = parseBacklog(
      git(["show", `${evidence}:${backlogPath}`], root),
    ).entries.find((entry) => entry.identity === dependency.supplier.identity);
    const linked = selected?.plan?.target;
    if (linked && values.plan && linked !== values.plan)
      throw new BacklogError(
        "Supplied plan disagrees with the supplier historical backlog.",
      );
    const planTarget = declared ?? linked ?? values.plan;
    if (planTarget !== undefined || preparation.approach?.kind === "planned") {
      const planPath =
        planTarget === undefined
          ? path
          : relative(root, pathFor(file, planTarget)).split(sep).join("/");
      const plan = readPlanSlices(
        git(["show", `${evidence}:${planPath}`], root),
      );
      if (
        plan.status !== "interpreted" ||
        !plan.slices.length ||
        plan.slices.some((slice) => slice.status !== "done")
      )
        throw new BacklogError(
          "Supplier outcome is incomplete: every planned slice must be done. Nothing was written.",
        );
    } else {
      if (!values["planless-complete"] || !values["completion-file"])
        throw new BacklogError(
          "Planless supplier needs an explicit outcome judgment and recoverable completion-file evidence. Nothing was written.",
        );
      const proof = relative(root, resolve(root, values["completion-file"]))
        .split(sep)
        .join("/");
      if (
        proof.startsWith("../") ||
        !git(["show", `${evidence}:${proof}`], root).trim()
      )
        throw new BacklogError(
          "Planless completion evidence must be readable in the supplier revision.",
        );
    }
    const receipt = `Condition ${dependency.state === "satisfied" ? "satisfied" : "remains unresolved"}: ${dependency.condition}\nAccepted integration: ${accepted} on ${values.remote}/${target}.`;
    const summary = dependency.resolution.summary;
    return {
      revision: evidence,
      path: locator,
      summary: summary.endsWith(receipt) ? summary : `${summary}\n${receipt}`,
    };
  });
  if (captured.threw) {
    if (captured.error instanceof BacklogError) throw captured.error;
    throw new BacklogError(
      `Supplier completion evidence could not be established: ${captured.error.message}. Nothing was written.`,
    );
  }
  return captured.result;
}
