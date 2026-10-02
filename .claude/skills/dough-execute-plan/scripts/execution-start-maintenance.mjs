import { refreshDefaultCheckout } from "./maintain-default-checkout.mjs";
import { remoteOf } from "./workspace-publication-ownership.mjs";

// Startup reports a local refresh failure separately from remote acceptance.
export function maintenance(request) {
  return refreshDefaultCheckout({
    checkout: request.integration,
    remote: remoteOf(request),
    integrationBranch: request.target,
  });
}

function outcome({ result, reason, error }) {
  return { result, ...(reason ? { reason } : {}), ...(error ? { error } : {}) };
}

function unresolved({ result }) {
  return result === "deferred" || result === "stopped";
}

// The start's local-refresh report: the latest outcome and its reason, plus
// an earlier distinct issue when the latest attempt also left the checkout
// unrefreshed. Checkout inventories stay out of the report; the decisions
// were made before this projection.
export function reportedMaintenance(latest, earlier) {
  const report = { maintenance: outcome(latest) };
  if (
    earlier &&
    earlier !== latest &&
    unresolved(earlier) &&
    unresolved(latest) &&
    (earlier.result !== latest.result || earlier.reason !== latest.reason)
  )
    report.earlierMaintenance = outcome(earlier);
  return report;
}
