import { isDirectCliEntry } from "./ci-direct-entry.mjs";

export {
  checkoutRoot,
  createMailbox,
  mailboxRoot,
  readMailbox,
  receiptPrefix,
} from "./ci-mailbox-location.mjs";
export {
  publishMailboxEvent,
  readDeliveryProgress,
  readMailboxEvents,
  readWorkerIdentity,
  recordDeliveryProgress,
  recordWorkerIdentity,
  workerLossReason,
} from "./ci-mailbox-store.mjs";
export {
  readRevisionCoverage,
  registerPushedRevision,
} from "./ci-mailbox-revision-records.mjs";
export { mailboxWorkerLoss } from "./ci-mailbox-worker-process.mjs";
export {
  completeRevision,
  requestMailboxStop,
} from "./ci-mailbox-complete.mjs";
export {
  probeMailbox,
  runMailboxWorker,
  startExecutionMailbox,
  streamMailboxWorker,
} from "./ci-mailbox-operations.mjs";

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  const { runMailboxCli } = await import("./ci-mailbox-cli.mjs");
  await runMailboxCli(process.argv.slice(2));
}
