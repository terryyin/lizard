#!/usr/bin/env node
// Applies one explicitly selected product backlog change to the project's
// backlog file. It applies a decision; it never decides value, priority,
// prerequisites, completion, or who may execute the work.

import { dirname } from "node:path";
import { parseArgs } from "node:util";
import { addQueueEntry } from "./product-backlog-add.mjs";
import { adoptIdentities } from "./product-backlog-adopt.mjs";
import {
  completeEntry,
  releaseAgentProfiles,
} from "./product-backlog-complete.mjs";
import { setDirection } from "./product-backlog-direction.mjs";
import { mergeBacklogs } from "./product-backlog-merge.mjs";
import { placeEntry } from "./product-backlog-place.mjs";
import { refreshEntry } from "./product-backlog-refresh.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import {
  options,
  readPlacement,
  readPlan,
  resolvePath,
} from "./product-backlog-request.mjs";
import {
  reportAdd,
  reportAdopt,
  reportComplete,
  reportDirection,
  reportMerge,
  reportPlace,
  reportRefresh,
  reportTake,
} from "./product-backlog-report.mjs";
import { applyToBacklog } from "./product-backlog-store.mjs";
import {
  readState,
  recordState,
} from "./product-backlog-story-state-command.mjs";
import { preparationRefusal } from "./product-backlog-story-state-home.mjs";
import { takeEntry } from "./product-backlog-take.mjs";
import { usage } from "./product-backlog-usage.mjs";

async function add(file, values) {
  const request = {
    identity: values.identity,
    title: values.title,
    href: values.link,
    backlogDirectory: dirname(file),
    ...readPlacement(values),
  };
  await applyToBacklog(file, (source) => addQueueEntry(source, request));
  console.log(reportAdd(request.identity, values.file));
}

// Applies one change whose report needs more than the published bytes. The
// operation returns the backlog to publish alongside what it did; only the
// bytes reach the write boundary, and the outcome comes back here so that the
// report is written from a change already on disk.
async function applyReportedChange(file, operate) {
  let outcome;
  await applyToBacklog(file, (source) => {
    outcome = operate(source);
    return outcome.source;
  });
  return outcome;
}

async function place(file, values) {
  const request = {
    identity: values.identity,
    returning: values.return,
    ...readPlacement(values),
  };
  const outcome = await applyReportedChange(file, (source) =>
    placeEntry(source, request),
  );

  console.log(reportPlace(outcome, values.file));
}

async function take(file, values) {
  const request = {
    identity: values.identity,
    plan: readPlan(values),
    backlogDirectory: dirname(file),
  };
  const outcome = await applyReportedChange(file, (source) =>
    takeEntry(source, request),
  );

  console.log(reportTake(outcome, values.file));
}

async function complete(file, values) {
  const outcome = await applyReportedChange(file, (source) =>
    completeEntry(source, { identity: values.identity }),
  );
  const released = releaseAgentProfiles(dirname(file), outcome.entry.identity);
  console.log(reportComplete({ ...outcome, released }, values.file));
}

// Dropping a reference is not on offer, so a caller who asks for it is told
// rather than having the option quietly ignored.
async function refresh(file, values) {
  if (values["no-plan"]) {
    throw new BacklogError(
      `refresh repoints a reference the entry already carries and never ` +
        `drops one, so --no-plan asks for something this operation does ` +
        `not do.`,
    );
  }
  const request = {
    identity: values.identity,
    title: values.title,
    href: values.link,
    plan: values.plan,
    backlogDirectory: dirname(file),
  };
  const outcome = await applyReportedChange(file, (source) =>
    refreshEntry(source, request),
  );

  console.log(reportRefresh(outcome, values.file));
}

// Every option this verb takes belongs to it alone, so what a direction
// request means — including which of them must be stated together — is read
// where the direction itself is owned rather than split across this boundary.
async function direction(file, values) {
  const request = {
    text: values.text,
    clearing: values.clear,
    expected: values.expect,
    expectingNone: values["expect-none"],
  };
  const outcome = await applyReportedChange(file, (source) =>
    setDirection(source, request),
  );

  console.log(reportDirection(outcome, values.file));
}

async function adopt(file, values) {
  if (!values.all) {
    throw new BacklogError(
      `adopt needs --all, so that recording identities for every active ` +
        `entry is always an explicit request.\n\n${usage}`,
    );
  }
  const outcome = await applyReportedChange(file, (source) =>
    adoptIdentities(source, dirname(file)),
  );

  console.log(reportAdopt(outcome, values.file));
}

// The destination's own bytes are deliberately not read. A merge is run on a
// file that may still hold whatever an integration left in it, so what the
// result should say comes from the three supplied versions alone; the write
// boundary still serializes this run against every other script writer, and a
// refused merge still leaves the destination exactly as it was.
async function merge(file, values) {
  const outcome = await applyReportedChange(file, () =>
    mergeBacklogs({
      ancestor: resolvePath(values.ancestor),
      branches: values.branch?.map(resolvePath),
    }),
  );

  console.log(reportMerge(outcome, values.file));
}

const operations = {
  add,
  place,
  take,
  complete,
  refresh,
  direction,
  adopt,
  merge,
  "record-state": recordState,
  "read-state": readState,
};

async function main(argv) {
  let parsed;
  try {
    parsed = parseArgs({ args: argv, options, allowPositionals: true });
  } catch (error) {
    throw new BacklogError(`${error.message}\n\n${usage}`);
  }
  const { values, positionals } = parsed;

  if (values.help) {
    console.log(usage);
    return;
  }
  const named = positionals.length === 1 ? positionals[0] : "";
  if (!Object.hasOwn(operations, named)) {
    throw new BacklogError(
      `Unknown operation: ${positionals.join(" ") || "(none)"}\n\n${usage}`,
    );
  }
  await operations[named](resolvePath(values.file), values);
}

try {
  await main(process.argv.slice(2));
} catch (error) {
  if (error instanceof BacklogError) {
    const named = process.argv[2];
    const refusal =
      named === "record-state" || named === "read-state"
        ? preparationRefusal(error)
        : error.refusal;
    console.error(refusal);
    process.exit(1);
  }
  throw error;
}
