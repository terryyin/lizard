// Project-owned configuration read shared by both public command projections.
import { readFileSync } from "node:fs";
import { resolve } from "node:path";

export function readTestEntrySettings(project) {
  const configuration = resolve(project, ".planning/open-dough.json");
  try {
    return {
      kind: "text",
      configuration,
      text: readFileSync(configuration, "utf8"),
    };
  } catch (error) {
    return {
      kind: error.code === "ENOENT" ? "absent" : "unreadable",
      configuration,
      message: error.message,
    };
  }
}
