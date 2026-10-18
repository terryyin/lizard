// The established session a caller hands to a native session it started with
// no workflow: where the conversation works and what the developer authorized
// in advance, in one fixed order. It names the workspace, its role (an
// isolated owned workspace or the default checkout), branch, remote, trunk
// target and landing intent, then the starting revision and fetched trunk
// when the caller knows them; optional fields it does not have are omitted,
// never blank. It names no tracking, identity, mode, or agent: a workflow
// invoked in the session later carries its own.
const required = [
  ["workspace", "workspace"],
  ["role", "workspace role"],
  ["branch", "branch"],
  ["remote", "remote"],
  ["target", "target"],
  ["landing", "landing"],
];
const optional = [
  ["startingRevision", "startingRevision"],
  ["fetched", "fetched"],
];

export function formatEstablishedSession(session) {
  return [
    "Established session:",
    ...[...required, ...optional.filter(([key]) => session[key])].map(
      ([key, label]) => `- ${label}: ${session[key]}`,
    ),
  ].join("\n");
}
