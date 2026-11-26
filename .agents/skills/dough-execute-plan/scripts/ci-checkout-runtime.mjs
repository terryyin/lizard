// Resolve the CI runtime from the invoking host's root in the selected checkout.
import { existsSync, realpathSync } from "node:fs";
import { dirname, join } from "node:path";

const skillName = "dough-execute-plan";
const entryRelative = join("scripts", "ci-mailbox.mjs");
const hookRelative = join("scripts", "ci-host-hook.mjs");

export function requireSelectedRuntimeEntrypoint(selectedRoot, skillRoot) {
  const selectedCheckout = realpathSync(selectedRoot);
  const entrypoint = join(skillRoot, entryRelative);
  if (!existsSync(entrypoint))
    throw new Error(
      `CI runtime is missing from selected checkout: ${entrypoint}`,
    );
  const runtimeCheckout = realpathSync(
    join(dirname(entrypoint), "../../../.."),
  );
  if (runtimeCheckout !== selectedCheckout)
    throw new Error(
      `CI runtime checkout ${runtimeCheckout} does not match selected checkout ${selectedCheckout}`,
    );
  return entrypoint;
}

export function resolveCheckoutRuntime(workspace, { host } = {}) {
  const selectedCheckout = realpathSync(workspace);
  const root = host === "claude" ? ".claude" : ".agents";
  const skillRoot = join(selectedCheckout, root, "skills", skillName);
  const entrypoint = requireSelectedRuntimeEntrypoint(
    selectedCheckout,
    skillRoot,
  );
  return {
    root,
    skillRoot,
    entrypoint,
    hookEntrypoint: join(skillRoot, hookRelative),
    checkout: selectedCheckout,
  };
}

export function checkoutRuntimeReceipt({ root, skillRoot, entrypoint }) {
  return { root, skillRoot, entrypoint };
}
