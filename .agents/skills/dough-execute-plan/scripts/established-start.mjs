// The established start a caller hands to an execution session once it has
// already run the start command: the instruction text that names the claim,
// workspace and published revision, in one fixed order. A one-shot start
// (`tracking: "one-shot"`) published nothing, so it names no publisher,
// published revision or agent; it names the workspace's role and the selected
// landing instead. Optional fields the caller does not have are omitted, never
// blank.
const required = [
  ["identity", "identity"],
  ["publisherId", "publisher ID"],
  ["workspace", "workspace"],
  ["branch", "branch"],
  ["mode", "mode"],
  ["remote", "remote"],
  ["target", "target"],
  ["publishedSha", "publishedSha"],
];
const optional = [
  ["agent", "agent"],
  ["plan", "plan"],
  ["startingRevision", "startingRevision"],
  ["candidateSha", "candidateSha"],
];
const oneShotRequired = [
  ["tracking", "tracking"],
  ["identity", "identity"],
  ["workspace", "workspace"],
  ["role", "workspace role"],
  ["branch", "branch"],
  ["mode", "mode"],
  ["remote", "remote"],
  ["target", "target"],
  ["landing", "landing"],
];
const oneShotOptional = [
  ["startingRevision", "startingRevision"],
  ["fetched", "fetched"],
];

export function formatEstablishedStart(start) {
  const [always, known] =
    start.tracking === "one-shot"
      ? [oneShotRequired, oneShotOptional]
      : [required, optional];
  const lines = [...always, ...known.filter(([key]) => start[key])].map(
    ([key, label]) => `- ${label}: ${start[key]}`,
  );
  return ["Established start:", ...lines].join("\n");
}
