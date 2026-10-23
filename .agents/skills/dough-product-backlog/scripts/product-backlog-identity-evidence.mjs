// The identity-evidence operation: what the project already shows about the
// identities it has used, so whoever allocates a new one can choose above all
// of it. It reads the active seed directory, the backlog's two lists, and the
// done records and catalog beside the backlog, and the Git history reachable
// from HEAD of the seed and done directories (which still shows seeds and
// records cleanup has since removed), and reports each source's highest seed
// number with the file, record, or adding commit behind it, the next number
// above every source read, and each source or file it could not read. A
// supplied candidate identity is reported as active, completed, in history,
// or unlisted.
//
// It only reads. It reserves, registers, and writes nothing: the caller still
// chooses the identity, and the canonical home records it.

import { dirname, join } from "node:path";
import {
  backlogSource,
  doneCatalogSource,
  doneRecordsSource,
  seedDirectorySource,
  seedNumberOf,
} from "./product-backlog-identity-sources.mjs";
import {
  historyFact,
  historySource,
} from "./product-backlog-identity-history.mjs";
import { readDoneRecordFiles } from "./product-backlog-complete.mjs";
import { resolvePath } from "./product-backlog-request.mjs";

// The seed directory a project keeps beside its backlog unless it names
// another with --seeds.
export const defaultSeedDirectory = "seeds";

// The highest seed number among `named`, each { identity, evidence } (and,
// from history, the adding `commit`) and, across sources, the source naming
// it; the first named wins a tie.
function highestOf(named) {
  let highest;
  for (const { identity, evidence, commit, source } of named) {
    const seed = seedNumberOf(identity);
    if (seed === undefined) continue;
    if (highest === undefined || seed.number > highest.number)
      highest = { ...seed, identity, evidence, commit, source };
  }
  return highest;
}

// A candidate is "active" while the backlog lists it, else "completed" while
// a done record names it, else "in-history" when history shows its own done
// record or its seed ID while no active seed carries that ID, else
// "unlisted". Whatever history shows of it is reported under `history` with
// every status.
function candidateStatus(identity, { seeds, backlog, records, history }) {
  const listed = backlog.named.find((entry) => entry.identity === identity);
  const record = records.named.find((each) => each.identity === identity);
  const completed =
    record === undefined
      ? {}
      : {
          completed: {
            record: record.evidence,
            title: record.title,
            completedAt: record.completedAt,
          },
        };
  const traced = historyFact(identity, history);
  const inHistory = traced === undefined ? {} : { history: traced };
  if (listed !== undefined)
    return {
      identity,
      status: "active",
      list: listed.list,
      ...completed,
      ...inHistory,
    };
  if (record !== undefined)
    return { identity, status: "completed", ...completed, ...inHistory };
  const seed = seedNumberOf(identity)?.seed;
  const activeSeed = seeds.named.some((entry) => entry.identity === seed);
  if (
    traced?.record !== undefined ||
    (traced?.seed !== undefined && !activeSeed)
  )
    return { identity, status: "in-history", ...inHistory };
  return { identity, status: "unlisted", ...inHistory };
}

function reportedHighest(highest) {
  if (highest === undefined) return null;
  const { seed, number, identity, evidence, commit } = highest;
  return {
    seed,
    number,
    identity,
    evidence,
    ...(commit === undefined ? {} : { commit }),
  };
}

/**
 * The allocation evidence beside the backlog at `file`, with the seed
 * directory at `seedDirectory` (default: "seeds" beside the backlog), the
 * history of both reachable from HEAD in the backlog's repository, and the
 * status of `identity` when one is supplied.
 */
export async function readIdentityEvidence(
  file,
  { seedDirectory = join(dirname(file), defaultSeedDirectory), identity },
) {
  const backlogDirectory = dirname(file);
  const doneFiles = readDoneRecordFiles(backlogDirectory);
  const seeds = seedDirectorySource(backlogDirectory, seedDirectory);
  const backlog = backlogSource(backlogDirectory, file);
  const records = doneRecordsSource(doneFiles);
  const history = await historySource(backlogDirectory, seedDirectory);
  const sources = [
    seeds,
    backlog,
    records,
    doneCatalogSource(backlogDirectory, doneFiles),
    history,
  ];
  const overall = highestOf(
    sources.flatMap(({ source, named }) =>
      named.map((entry) => ({ ...entry, source })),
    ),
  );
  return {
    highest:
      overall === undefined
        ? null
        : { source: overall.source, ...reportedHighest(overall) },
    next:
      overall === undefined
        ? null
        : `${overall.prefix}${String(overall.number + 1).padStart(overall.width, "0")}`,
    sources: sources.map(({ source, path, revision, read, named }) => ({
      source,
      path,
      ...(revision === undefined ? {} : { revision }),
      read,
      highest: reportedHighest(highestOf(named)),
    })),
    gaps: sources.flatMap(({ source, gaps }) =>
      gaps.map((gap) => ({ source, ...gap })),
    ),
    ...(identity === undefined
      ? {}
      : {
          candidate: candidateStatus(identity, {
            seeds,
            backlog,
            records,
            history,
          }),
        }),
  };
}

// The identity-evidence command: prints the report as JSON, writing nothing
// and taking no lock, since a reader never needs to wait for a writer.
export async function printIdentityEvidence(file, values) {
  const evidence = await readIdentityEvidence(file, {
    seedDirectory: resolvePath(values.seeds),
    identity: values.identity,
  });
  console.log(JSON.stringify(evidence, null, 2));
}
