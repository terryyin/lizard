#!/usr/bin/env node
// The one CLI shape every Git-aware backlog adapter shares: parse the same
// four options plus whichever extra ones an adapter declares, resolve the
// real repository root, dispatch to this adapter's own primary operation
// (`merge`, `rebase`, ...), the shared `continue`, or an adapter's own
// `validate`, print the outcome, and exit nonzero for whichever statuses this
// adapter treats as failing — the same recovery discipline every adapter
// reports through. Only the primary operation's name and handler, its usage
// text, its failing-status list, and any adapter-specific options/verbs are
// adapter-specific; everything else here was identical, line for line,
// between `product-backlog-git-merge.mjs` and `product-backlog-git-rebase.mjs`
// before this was shared.
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

async function dispatch({
  argv,
  primaryName,
  usage,
  primaryOperation,
  continueOperation,
  validateOperation,
  failingStatuses,
  extraOptions,
}) {
  const { values, positionals } = parseArgs({
    args: argv,
    options: { ...options, ...extraOptions },
    allowPositionals: true,
  });
  if (values.help) {
    console.log(usage);
    return true;
  }
  const repoRoot = repositoryRoot(resolve(values.cwd));
  const named = positionals.length === 1 ? positionals[0] : "";

  // Whichever extra, adapter-declared options were actually parsed, passed
  // through by their own names rather than assumed by this shared shape —
  // `mergeOperation`/`continueOperation` simply never destructure a name this
  // adapter did not declare.
  const extra = Object.fromEntries(
    Object.keys(extraOptions ?? {}).map((name) => [name, values[name]]),
  );

  let outcome;
  if (named === primaryName) {
    if (!values.ref) {
      throw new BacklogError(`Supply --ref <ref>.\n\n${usage}`);
    }
    outcome = await primaryOperation({
      repoRoot,
      file: values.file,
      ref: values.ref,
      ...extra,
    });
  } else if (named === "continue") {
    outcome = await continueOperation({ repoRoot, file: values.file });
  } else if (named === "validate" && validateOperation) {
    outcome = await validateOperation({ repoRoot, file: values.file });
  } else {
    throw new BacklogError(
      `Unknown operation: ${positionals.join(" ") || "(none)"}\n\n${usage}`,
    );
  }

  console.log(outcome.message ?? outcome.status);
  return !failingStatuses.includes(outcome.status);
}

// Runs one adapter's CLI to completion against real `process.argv`: this
// adapter's own refusal is reported on stderr and exits nonzero; any other
// error is rethrown unchanged — the same top-level handling each adapter's
// own `main`/try-catch pair used to duplicate before this was shared. What
// Git, the merge driver, and hooks wrote to stderr during the operation is
// collected rather than forwarded: a successful operation shows only its
// receipt, while every other outcome — a failing status, a refusal, or an
// unexpected error — passes that collected text on to this process's stderr.
export async function runGitOperationCli(config) {
  const run = await collectingGitDiagnostics(() => dispatch(config));
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
