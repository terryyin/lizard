// The project a lead reading reads (./ci-proof-leads.mjs): its checkout
// root, its configured CI (../../dough-execute-plan/scripts/
// ci-project-configuration.mjs) and repository, the requested revision, its
// Intent locations, which are the default scope, and what its summary
// records of the test selection.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { readCiConfiguration } from "../../dough-execute-plan/scripts/ci-project-configuration.mjs";
import * as revisions from "./ci-proof-revisions.mjs";

const settingsPath = ".planning/open-dough.json";

// The Intent locations ROOT's settings declare, in their order.
function intentLocations(root) {
  let settings;
  try {
    settings = JSON.parse(readFileSync(join(root, settingsPath), "utf8"));
  } catch {
    return [];
  }
  const locations = settings?.intent?.locations;
  return Array.isArray(locations)
    ? locations.filter((at) => typeof at?.path === "string" && at.path)
    : [];
}

// A scope path as a repository path: no leading `./` or trailing `/`.
const repositoryPath = (path) => path.replace(/^\.\//, "").replace(/\/+$/, "");

/**
 * REQUEST's project: the checkout holding its `directory`, the requested
 * `revision` (default HEAD), its repository (`repository`, else the origin
 * remote's), its configured CI, asked of GitHub through GITHUB or run in
 * the checkout, its Intent locations, and the scope: the request's paths,
 * else those locations, else undefined for every entry a summary holds.
 * Rejected when any of these cannot be known.
 * @param {{
 *   directory: string;
 *   revision?: string;
 *   scope?: readonly string[];
 *   repository?: string;
 * }} request
 * @param {import("./ci-proof-read.mjs").GitHubAsks} gitHub
 */
export async function projectOf(request, gitHub) {
  const root = await revisions.checkoutRoot(request.directory);
  const settings = readCiConfiguration(root);
  const spec = request.revision ?? "HEAD";
  const revision = await revisions.resolvedRevision(root, spec);
  const repository =
    request.repository ?? (await revisions.originRepository(root));
  if (repository === undefined)
    throw new Error("the origin remote names no OWNER/REPO; pass --repository");
  const ci =
    settings.adapter === undefined
      ? { repository, workflow: settings.workflow, gitHub }
      : {
          repository,
          adapter: settings.adapter,
          branch: await revisions.defaultBranch(root),
          root,
        };
  const locations = intentLocations(root);
  let scope;
  if (request.scope?.length) scope = request.scope.map(repositoryPath);
  else if (locations.length > 0) scope = locations.map((at) => at.path);
  return { root, requested: { spec, revision }, ci, locations, scope };
}

/**
 * What the summary records of the test selection for CI: no command, with
 * where to read it, and each in-scope entry's declared location and, for
 * each unit it holds results in, that unit's part statuses, from ENTRIES
 * (./ci-proof-scope.mjs) and LOCATIONS.
 * @param {import("./ci-proof-attempts.mjs").ProjectCi} ci
 * @param {readonly import("./ci-proof-scope.mjs").ScopeEntry[]} entries
 * @param {readonly { path: string; kind?: unknown; include?: unknown }[]} locations
 */
export function selectionOf(ci, entries, locations) {
  const configuration =
    ci.adapter !== undefined
      ? "the CI configuration behind this project's ciAdapter"
      : /^\d+$/.test(ci.workflow ?? "")
        ? `the file of GitHub Actions workflow ${ci.workflow}`
        : `.github/workflows/${ci.workflow}`;
  const declared = (path) => {
    const location = locations.find((at) => at.path === path);
    if (!location) return "not an Intent location";
    if (typeof location.kind !== "string") return "sources";
    const include = Array.isArray(location.include) ? location.include : [];
    return [location.kind, ...include].join(" ");
  };
  return {
    text: `test-selection command not recorded in the summary; remedy: read it from ${configuration}`,
    entries: entries.map((entry) => ({
      path: entry.path,
      declared: declared(entry.path),
      units: entry.units.map(
        ({ of, present, missing, ambiguous, ...unit }) => ({
          ...unit,
          parts: [
            `${present} of ${of} parts present`,
            ...(missing.length ? [`${missing.length} missing`] : []),
            ...(ambiguous.length ? [`${ambiguous.length} ambiguous`] : []),
          ].join(", "),
        }),
      ),
    })),
  };
}
