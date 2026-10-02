// The established preparation a caller hands to a refinement session once it
// has already run the preparation start: the instruction text that names the
// assignment and its workspace, in one fixed order. A one-shot preparation
// (`tracking: "one-shot"`) published no assignment, so it names no agent or
// published revision; it names the workspace's role and the selected landing
// instead. Optional fields the caller does not have are omitted, never blank.
const required = [
  ["identity", "identity"],
  ["workspace", "workspace"],
  ["branch", "branch"],
  ["remote", "remote"],
  ["target", "target"],
  ["agent", "agent"],
];
const optional = [
  ["publishedSha", "publishedSha"],
  ["integration", "integration checkout"],
];
const oneShotRequired = [
  ["tracking", "tracking"],
  ["identity", "identity"],
  ["workspace", "workspace"],
  ["role", "workspace role"],
  ["branch", "branch"],
  ["remote", "remote"],
  ["target", "target"],
  ["landing", "landing"],
];
const oneShotOptional = [
  ["startingRevision", "startingRevision"],
  ["integration", "integration checkout"],
];

export function formatEstablishedPreparation(preparation) {
  const [always, known] =
    preparation.tracking === "one-shot"
      ? [oneShotRequired, oneShotOptional]
      : [required, optional];
  const lines = [...always, ...known.filter(([key]) => preparation[key])].map(
    ([key, label]) => `- ${label}: ${preparation[key]}`,
  );
  return ["Established preparation:", ...lines].join("\n");
}
