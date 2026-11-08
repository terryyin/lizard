// The rule that an agent profile never belongs among the done records beside
// a backlog: one found there is misplaced, and its home is the profile
// directory under the same file name. Readers supply the files they found;
// nothing is moved. No filesystem, Git, or Node-only imports, so a merge
// adapter reading its index and a publisher reading a candidate share it.

import {
  agentProfileDirectory,
  parseAgentProfileFile,
  profileAgentName,
} from "./product-backlog-agent-profile.mjs";
import { doneRecordDirectory } from "./product-backlog-done-record.mjs";

// Whether a file in the done-record directory is named as an agent profile,
// so its text decides whether it is one misplaced there. A reader reads only
// such files to apply misplacedAgentProfiles.
export function hasAgentProfileFileName(fileName) {
  return profileAgentName(fileName) !== undefined;
}

// `files` are the files ({ fileName, text }) in the done-record directory
// beside the backlog whose directory is `directory` (relative to the
// repository, "." at its root). Returns each misplaced profile's path and
// home, relative to the repository.
export function misplacedAgentProfiles(directory, files) {
  const beside = (path) =>
    directory === "." || directory === "" ? path : `${directory}/${path}`;
  return files
    .filter(
      ({ fileName, text }) =>
        hasAgentProfileFileName(fileName) &&
        parseAgentProfileFile(fileName, text).ok,
    )
    .map(({ fileName }) => ({
      path: beside(`${doneRecordDirectory}/${fileName}`),
      home: beside(`${agentProfileDirectory}/${fileName}`),
    }));
}

// One line per misplaced profile, naming it and the path it belongs at.
export function describeMisplacedAgentProfiles(misplaced) {
  return misplaced
    .map(
      ({ path, home }) => `${path} is an agent profile; it belongs at ${home}.`,
    )
    .join("\n");
}
