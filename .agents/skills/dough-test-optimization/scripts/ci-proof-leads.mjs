// Ranks test optimization leads from this project's retained CI proof and
// prints them as one bounded table (./ci-proof-leads-table.mjs), so an agent
// never loads a raw summary. The summary is the configured CI's for the
// attempt chosen for the requested revision (./ci-proof-attempts.mjs), or a
// file the project retains elsewhere (--summary), used only when it is that
// attempt's (./ci-proof-gaps.mjs). The scope's items are ranked within each
// unit (./ci-proof-scope.mjs, ./ci-proof-ranking.mjs).
import { resolve } from "node:path";
import { parseArgs } from "node:util";
import { isDirectCliEntry } from "../../dough-execute-plan/scripts/ci-direct-entry.mjs";
import {
  askGitHubArchive,
  readGitHubActions,
} from "../../dough-execute-plan/scripts/ci-github-actions.mjs";
import * as attempts from "./ci-proof-attempts.mjs";
import * as evidenceGaps from "./ci-proof-gaps.mjs";
import { leadsTable } from "./ci-proof-leads-table.mjs";
import { projectOf, selectionOf } from "./ci-proof-project.mjs";
import { LEAD_LIMIT, RANKED_UNITS, rankedLeads } from "./ci-proof-ranking.mjs";
import { ancestryOf } from "./ci-proof-revisions.mjs";
import { scopeOf } from "./ci-proof-scope.mjs";

const USAGE = `Usage: node <this skill>/scripts/ci-proof-leads.mjs [--revision <rev>] [--scope <path>]... [--summary <file>] [--repository <owner/repo>]

Run from this project's checkout. Prints a bounded table of test optimization
leads from the CI proof this project retains: the attempt read and how its
revision relates to the requested one (default HEAD), the recorded test
selection, gaps with remedies, per-unit totals, and, within each of up to
${RANKED_UNITS} units, leads ranked by summed CI time (up to ${LEAD_LIMIT} in all), the
folders holding most of the rest, and the remaining tail.
The scope defaults to this project's Intent locations
(.planning/open-dough.json). --summary reads a summary this project retains
elsewhere instead of asking CI for it; when CI names the attempt for the
revision, the summary must be that attempt's. --repository names OWNER/REPO
when the origin remote does not. Exits 0 when a summary ranked, 1 when none
could (the table says why), and 2 on a usage error.
`;

// The attempt CI chooses for the requested revision, and the gaps of that
// choice; a failure to ask CI is a gap with its remedy.
async function choiceOf(ci, requested, ancestry, signal, gaps) {
  const name = attempts.ciName(ci);
  try {
    if (ci.adapter !== undefined && ci.branch === undefined)
      throw new Error(
        "no branch to ask its discover for: check out a branch or set origin's HEAD",
      );
    const listed = await attempts.listedAttempts(ci, signal);
    const choice = await attempts.chosenAttempt(listed, requested, ancestry);
    gaps.push(...evidenceGaps.choiceGaps(choice));
    if (!choice.chosen)
      gaps.push({
        reason: `${name} lists no completed attempt at the requested revision or an ancestor among its ${listed.length} newest`,
        remedy: evidenceGaps.summaryRemedy(ci),
      });
    return choice.chosen;
  } catch (error) {
    gaps.push({
      reason: `${name} could not list its attempts: ${evidenceGaps.failureText(error)}`,
      remedy: evidenceGaps.summaryRemedy(ci),
    });
    return undefined;
  }
}

// The summary of ATTEMPT, from REQUEST's file or from CI, its source, and
// the attempt it names.
async function summaryOf(request, ci, attempt, signal, gaps) {
  const name = attempts.ciName(ci);
  const source = attempts.ciSource(ci);
  if (request.summaryFile !== undefined) {
    const file = resolve(request.directory, request.summaryFile);
    const read = evidenceGaps.fileSummary(file, ci.repository, attempt);
    const named =
      !attempt && read.summary ? evidenceGaps.namedBy(read.summary) : undefined;
    const confirmed = attempt
      ? `confirmed as the attempt ${name} names`
      : "its attempt as it names it, unconfirmed by CI";
    return { read, source: `summary file ${file}, ${confirmed}`, named };
  }
  if (!attempt) return { source };
  const remedy = evidenceGaps.summaryRemedy(ci, attempt);
  try {
    const read = await attempts.readAttemptSummary(ci, attempt, signal);
    if (read.summary === undefined)
      gaps.push({
        reason: `${name} gave no usable summary for ${attempts.identityOf(attempt)}`,
        remedy,
      });
    return { read, source };
  } catch (error) {
    gaps.push({
      reason: `${name} could not give ${attempts.identityOf(attempt)}'s summary: ${evidenceGaps.failureText(error)}`,
      remedy,
    });
    return { source };
  }
}

/**
 * One lead reading of REQUEST's project (`directory` inside its checkout,
 * optional `revision`, `scope` paths, `summaryFile` and `repository`),
 * asking GitHub through GITHUB: its table, and whether a summary ranked.
 * Rejected when the project cannot be read.
 * @param {Parameters<typeof projectOf>[0] & { summaryFile?: string }} request
 * @param {{ gitHub?: import("./ci-proof-read.mjs").GitHubAsks, signal?: AbortSignal }} [asks]
 */
export async function leadsReport(
  request,
  {
    gitHub = { readGitHubActions, askGitHubArchive },
    signal = new AbortController().signal,
  } = {},
) {
  const { root, requested, ci, locations, scope } = await projectOf(
    request,
    gitHub,
  );
  const gaps = [];
  const ancestry = ancestryOf(root, requested.revision);
  const chosen = await choiceOf(ci, requested.revision, ancestry, signal, gaps);
  const { read, source, named } = await summaryOf(
    request,
    ci,
    chosen,
    signal,
    gaps,
  );
  const attempt = chosen ?? named;
  const report = {
    repository: ci.repository,
    requested,
    source,
    gaps,
    ...(attempt && {
      attempt: {
        identity: attempts.identityOf(attempt),
        revision: attempt.revision,
        outcome: attempt.conclusion ?? attempt.status,
      },
    }),
  };
  if (read?.summary === undefined) {
    const verdict = read
      ? evidenceGaps.refusalText(read, attempt)
      : "no summary could be read; see the gaps";
    return { text: leadsTable({ ...report, verdict }), ranked: false };
  }
  const held = scopeOf(read.summary, scope);
  if (ci.adapter && !request.summaryFile && held.noBreakdown.length > 0)
    gaps.push({
      reason: `${attempts.ciName(ci)}'s proof answer carries no file breakdown for ${held.noBreakdown.join(", ")}`,
      remedy: `${evidenceGaps.summaryRemedy(ci, attempt)}, as the full summary may carry one`,
    });
  gaps.push(...held.gaps);
  const compatibility = await evidenceGaps.compatibilityOf(
    root,
    attempt.revision,
    requested.revision,
    ancestry,
    scope ?? held.entries.map((entry) => entry.path),
  );
  const selection = selectionOf(ci, held.entries, locations);
  const ranking = rankedLeads(held.items);
  return {
    text: leadsTable({ ...report, compatibility, selection, ranking }),
    ranked: true,
  };
}

async function main(argv) {
  let values;
  try {
    ({ values } = parseArgs({
      args: argv,
      options: {
        revision: { type: "string" },
        scope: { type: "string", multiple: true },
        summary: { type: "string" },
        repository: { type: "string" },
        help: { type: "boolean", short: "h" },
      },
    }));
  } catch (error) {
    process.stderr.write(`ci-proof-leads: ${error.message}\n${USAGE}`);
    return 2;
  }
  if (values.help) {
    process.stdout.write(USAGE);
    return 0;
  }
  try {
    const { text, ranked } = await leadsReport({
      directory: process.cwd(),
      revision: values.revision,
      scope: values.scope,
      summaryFile: values.summary,
      repository: values.repository,
    });
    process.stdout.write(text);
    return ranked ? 0 : 1;
  } catch (error) {
    process.stderr.write(`ci-proof-leads: ${error.message}\n`);
    return 2;
  }
}

if (isDirectCliEntry(import.meta.url, process.argv[1]))
  process.exitCode = await main(process.argv.slice(2));
