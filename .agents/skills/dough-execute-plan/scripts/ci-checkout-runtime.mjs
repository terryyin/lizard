// Resolve the checkout-bound CI runtime entry point. Prefer a host alias when
// it exists; otherwise use any usable same-checkout installation.
import { existsSync, realpathSync } from "node:fs";
import { dirname, join } from "node:path";

const skillName = "dough-execute-plan";
const entryRelative = join("scripts", "ci-mailbox.mjs");
const deliveryRelative = join("scripts", "execution-increment-delivery.mjs");
const hookRelative = join("scripts", "ci-host-hook.mjs");

const aliasRoots = {
  cursor: [".agents"],
  claude: [".claude", ".agents"],
  codex: [".agents"],
};

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
  const hostOrder = aliasRoots[host] ?? [".agents", ".claude"];
  const aliases = [...new Set([...hostOrder, ".agents", ".claude"])];
  const tried = [];
  for (const alias of aliases) {
    const skillRoot = join(selectedCheckout, alias, "skills", skillName);
    const entrypoint = join(skillRoot, entryRelative);
    tried.push(entrypoint);
    if (!existsSync(entrypoint)) continue;
    try {
      requireSelectedRuntimeEntrypoint(selectedCheckout, skillRoot);
    } catch {
      continue;
    }
    return {
      alias,
      skillRoot,
      entrypoint,
      deliveryEntrypoint: join(skillRoot, deliveryRelative),
      hookEntrypoint: join(skillRoot, hookRelative),
      checkout: selectedCheckout,
    };
  }
  throw new Error(
    `CI runtime is missing from selected checkout (tried ${tried.join(", ")})`,
  );
}
