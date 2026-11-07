import { readFileSync, writeFileSync } from "node:fs";

const BASE = "website/frontend";
const [reportPath, timesPath] = process.argv.slice(2);
const report = JSON.parse(readFileSync(reportPath, "utf8"));
const times = [];

const statusOf = (results) => {
  const statuses = results.map((result) => result?.status);
  if (statuses.includes("failed")) return "failed";
  if (statuses.length > 0 && statuses.every((status) => status === "passed")) return "passed";
  return "other";
};

for (const feature of report) {
  const path = `${BASE}/${feature.uri}`;
  for (const scenario of feature.elements ?? []) {
    if (scenario.type !== "scenario") continue;
    const results = [
      ...(scenario.before ?? []),
      ...(scenario.steps ?? []),
      ...(scenario.after ?? []),
    ].map((item) => item.result);
    console.log(`${statusOf(results)}\t${path}`);

    if (timesPath === undefined) continue;
    const durations = results
      .map((result) => result?.duration)
      .filter((duration) => typeof duration === "number");
    if (durations.length > 0) {
      const seconds = durations.reduce((total, duration) => total + duration, 0) / 1e9;
      times.push(`${seconds}\t${path}\n`);
    }
  }
}

if (timesPath !== undefined) writeFileSync(timesPath, times.join(""), "utf8");
