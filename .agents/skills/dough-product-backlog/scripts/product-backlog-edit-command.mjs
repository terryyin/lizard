import { dirname } from "node:path";
import {
  directionHeading,
  setDirection,
} from "./product-backlog-direction.mjs";
import { refreshEntry } from "./product-backlog-refresh.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { applyReportedChange } from "./product-backlog-store.mjs";

export async function refresh(file, values) {
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
  const { identity, list } = outcome.entry;
  if (outcome.changed.length === 0) {
    console.log(
      `"${identity}" already reads as requested in ${values.file}, unchanged.`,
    );
    return;
  }
  console.log(
    `Refreshed the ${outcome.changed.join(" and ")} of "${identity}" in ` +
      `"## ${list}" in ${values.file}; its identity and its place are unchanged.`,
  );
}

export async function direction(file, values) {
  const request = {
    text: values.text,
    clearing: values.clear,
    expected: values.expect,
    expectingNone: values["expect-none"],
  };
  const outcome = await applyReportedChange(file, (source) =>
    setDirection(source, request),
  );
  const said = {
    set: `Set the "## ${directionHeading}" in ${values.file}`,
    replaced: `Replaced the "## ${directionHeading}" in ${values.file}`,
    cleared: `Cleared the "## ${directionHeading}" from ${values.file}`,
    unchanged: `The "## ${directionHeading}" in ${values.file} already reads as requested`,
  }[outcome.result];
  console.log(`${said}. Every entry in both lists is unchanged.`);
}
