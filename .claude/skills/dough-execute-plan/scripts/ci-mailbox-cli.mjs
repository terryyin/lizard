import { readMailbox, receiptPrefix } from "./ci-mailbox-location.mjs";
import {
  probeMailbox,
  runMailboxWorker,
  startExecutionMailbox,
  streamMailboxWorker,
} from "./ci-mailbox-operations.mjs";
import { registerPushedRevision } from "./ci-mailbox-revision-records.mjs";
import { awaitRevision, writeRevisionReceipt } from "./ci-mailbox-await.mjs";
import { completeRevision, stopMailbox } from "./ci-mailbox-complete.mjs";
import {
  acknowledgeDelivery,
  waitForTerminalResult,
} from "./ci-mailbox-store.mjs";
import { executionBudgetMs } from "./watch-ci-execution.mjs";

const resultPrefix = "CI_OBSERVER_RESULT ";

// Removes `--coordinator VALUE` from a stream command's arguments and returns
// the value.
function takeCoordinator(args) {
  const index = args.indexOf("--coordinator");
  if (index === -1) return undefined;
  const [, value] = args.splice(index, 2);
  if (!value || value.startsWith("--"))
    throw new Error("Expected --coordinator VALUE");
  return value;
}

export async function runMailboxCli([command, ...args]) {
  const coordinator = command === "stream" ? takeCoordinator(args) : undefined;
  if (command === "worker") {
    await runMailboxWorker(args[0]);
  } else if (["start", "stream"].includes(command)) {
    const [, repo, branch, budget] = args;
    const request = {
      mode: args[0] === "--execution" ? "execution" : undefined,
      repo,
      branch,
      maxDurationMs: budget ? Number(budget) : executionBudgetMs,
    };
    if (command === "stream") {
      const directory = await streamMailboxWorker(request, {
        write: (output) => process.stdout.write(output),
        stopOnSignal: true,
        coordinator,
      });
      const terminal = await waitForTerminalResult(directory);
      process.stdout.write(
        `${resultPrefix}${JSON.stringify({ directory, terminal })}\n`,
      );
    } else {
      const directory = await startExecutionMailbox(request);
      process.stdout.write(
        `${receiptPrefix}${JSON.stringify({ directory })}\n`,
      );
    }
  } else if (command === "probe") {
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory: probeMailbox() })}\n`,
    );
  } else if (command === "register-push") {
    const [directory, sha] = args;
    readMailbox(directory);
    const revision = registerPushedRevision(directory, sha);
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory, revision })}\n`,
    );
  } else if (command === "acknowledge") {
    const [directory, sequence] = args;
    readMailbox(directory);
    const deliveredThrough = acknowledgeDelivery(directory, Number(sequence));
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory, deliveredThrough })}\n`,
    );
  } else if (command === "await-revision") {
    const [directory, sha] = args;
    await writeRevisionReceipt(awaitRevision, directory, sha);
  } else if (command === "complete-revision") {
    const [directory, sha] = args;
    await writeRevisionReceipt(completeRevision, directory, sha);
  } else if (command === "stop") {
    const directory = args[0];
    const terminal = await stopMailbox(directory);
    process.stdout.write(
      `${receiptPrefix}${JSON.stringify({ directory, terminal })}\n`,
    );
  } else {
    throw new Error(
      "Usage: ci-mailbox.mjs probe | start --execution OWNER/REPO BRANCH [BUDGET_MS] | stream --execution OWNER/REPO BRANCH [BUDGET_MS] [--coordinator VALUE] | register-push DIRECTORY SHA | acknowledge DIRECTORY SEQUENCE | await-revision DIRECTORY SHA | complete-revision DIRECTORY SHA | stop DIRECTORY",
    );
  }
}
