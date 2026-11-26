import { dirname } from "node:path";
import {
  closeBesideBacklog,
  completeEntry,
  rebuildDoneCatalog,
} from "./product-backlog-complete.mjs";
import { intentCoverageError } from "./product-backlog-done-record.mjs";
import { BacklogError } from "./product-backlog-refusal.mjs";
import { resolvePath } from "./product-backlog-request.mjs";
import {
  applyReportedChange,
  holdingBacklog,
  readFile,
} from "./product-backlog-store.mjs";

// When a `complete` request runs. DOUGH_BACKLOG_COMPLETION_TIME, an ISO time,
// stands in for the clock so a check can place completions in time.
function readCompletionTime() {
  const supplied = process.env.DOUGH_BACKLOG_COMPLETION_TIME;
  if (supplied === undefined || supplied === "") return new Date();
  const time = new Date(supplied);
  if (Number.isNaN(time.getTime())) {
    throw new BacklogError(
      `DOUGH_BACKLOG_COMPLETION_TIME must be an ISO time: ${supplied}`,
    );
  }
  return time;
}

// Resolve and validate completion input before applying any backlog change.
function readIntentCoverage(values) {
  const path = resolvePath(values["intent-coverage"]);
  const reason = values["intent-coverage-unavailable"];
  if (path === undefined && reason === undefined) return undefined;
  if (path !== undefined && reason !== undefined) {
    throw new BacklogError(
      "Supply only one of --intent-coverage or --intent-coverage-unavailable.",
    );
  }
  if (values.dropped) {
    throw new BacklogError("Intent Coverage requires finished work.");
  }
  const result =
    path === undefined
      ? { unavailable: reason }
      : {
          report: readFile(
            path,
            `Could not find the Intent Coverage report file at ${path}.`,
          ),
        };
  const error = intentCoverageError(result);
  if (error) throw new BacklogError(error);
  return result;
}

// Keep the backlog hold through record writes and the catalog rebuild.
export async function complete(file, values) {
  const now = readCompletionTime();
  const intentCoverage = readIntentCoverage(values);
  const outcome = await applyReportedChange(
    file,
    (source) => completeEntry(source, { identity: values.identity }),
    ({ entry }) =>
      closeBesideBacklog(dirname(file), {
        entry,
        dropped: values.dropped,
        now,
        intentCoverage,
      }),
  );
  console.log(reportComplete(outcome, values.file));
}

// Rebuilds the done catalog under the backlog's lock, changing nothing else.
export async function catalogDone(file, values) {
  const catalog = await holdingBacklog(file, () =>
    rebuildDoneCatalog(dirname(file)),
  );
  console.log(reportCatalogDone(catalog, values.file));
}

// What a rebuild did to the done catalog, in a sentence that ends the
// report of the operation that rebuilt it. Unreadable record files are named:
// the catalog keeps them as gaps with no completion time.
function catalogSentence(catalog) {
  const unreadable =
    catalog.unreadable.length === 0
      ? ""
      : ` It names ${catalog.unreadable.length} unreadable done record${
          catalog.unreadable.length === 1 ? "" : "s"
        } without a completion time: ${catalog.unreadable.join(", ")}.`;
  const listed = `${catalog.records} done record${catalog.records === 1 ? "" : "s"}`;
  switch (catalog.status) {
    case "written":
      return ` Rebuilt done catalog ${catalog.path} beside the backlog, listing ${listed}.${unreadable}`;
    case "unchanged":
      return ` Done catalog ${catalog.path} beside the backlog already listed ${listed}.${unreadable}`;
    case "removed":
      return ` Removed done catalog ${catalog.path} beside the backlog; no done record remains.`;
    default:
      return "";
  }
}

function reportComplete(outcome, file) {
  const { identity, list, href } = outcome.entry;
  const released = outcome.released.map(
    ({ path }) => ` Released agent profile ${path} beside the backlog.`,
  );
  const expired = outcome.expired.map(
    (path) => ` Removed expired done record ${path} beside the backlog.`,
  );
  return (
    `Removed "${identity}" from "## ${list}" in ${file}. ` +
    `Its canonical home ${href} was not changed.${released.join("")}` +
    `${
      outcome.record === undefined
        ? " Removed it as dropped work; done records hold finished work only."
        : ` Wrote done record ${outcome.record} beside the backlog.`
    }${expired.join("")}${catalogSentence(outcome.catalog)}`
  );
}

function reportCatalogDone(catalog, file) {
  const unchanged = `${file} and its done records were not changed.`;
  if (catalog.status === "absent")
    return `No done record is beside ${file}, so no done catalog is published. ${unchanged}`;
  return `${catalogSentence(catalog).trimStart()} ${unchanged}`;
}
