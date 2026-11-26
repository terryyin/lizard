// Shared CLI for the installed merge and rebase commands.
import { resolve } from "node:path";
import { parseArgs } from "node:util";
import {
  collectingGitDiagnostics,
  repositoryRoot,
} from "./product-backlog-git-repository.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { defaultBacklogPath } from "./product-backlog-store.mjs";

const options = {
  file: { type: "string", default: defaultBacklogPath },
  ref: { type: "string" },
  cwd: { type: "string", default: "." },
  help: { type: "boolean", default: false },
};

const rebaseOptions = { onto: { type: "string" }, branch: { type: "string" } };

const failingStatuses = [
  "blocked",
  "catalog-uncommitted",
  "conflict",
  "disputed",
  "refused",
  "refused-before-commit",
];

function usageFor(name) {
  const entry = `product-backlog-git-${name}.mjs`;
  const location = "[--file <path>] [--cwd <dir>]";
  const range =
    name === "rebase"
      ? "--ref <upstream> [--onto <newbase>] [--branch <branch>]\n    "
      : "--ref <ref> ";
  return (
    `Usage:\n  ${entry} ${name} ${range}${location}\n` +
    `  ${entry} continue ${location}\n${
      name === "rebase" ? `  ${entry} validate ${location}\n` : ""
    }`
  );
}

async function dispatch(name, { start, resume, validate }) {
  const { values, positionals } = parseArgs({
    args: process.argv.slice(2),
    options: name === "rebase" ? { ...options, ...rebaseOptions } : options,
    allowPositionals: true,
  });
  const usage = usageFor(name);
  if (values.help) {
    console.log(usage);
    return true;
  }
  const repoRoot = repositoryRoot(resolve(values.cwd));
  const verb = positionals.length === 1 ? positionals[0] : "";
  const target = { repoRoot, file: values.file };

  let outcome;
  if (verb === name) {
    if (!values.ref) {
      throw new BacklogError(`Supply --ref <ref>.\n\n${usage}`);
    }
    outcome = await start({
      ...target,
      ref: values.ref,
      onto: values.onto,
      branch: values.branch,
    });
  } else if (verb === "continue") {
    outcome = await resume(target);
  } else if (verb === "validate" && validate) {
    outcome = await validate(target);
  } else {
    throw new BacklogError(
      `Unknown operation: ${positionals.join(" ") || "(none)"}\n\n${usage}`,
    );
  }

  console.log(outcome.message ?? outcome.status);
  return !failingStatuses.includes(outcome.status);
}

// Successful commands print only their receipt. Refusals and failures also
// carry Git, driver and hook diagnostics.
export async function runGitOperationCli(name, operation) {
  const run = await collectingGitDiagnostics(() => dispatch(name, operation));
  const succeeded = !run.threw && run.result;
  if (succeeded) {
    return;
  }
  process.stderr.write(run.stderr);
  process.exitCode = 1;
  if (!run.threw) {
    return;
  }
  if (!(run.error instanceof BacklogError)) {
    throw run.error;
  }
  console.error(run.error.refusal);
}
