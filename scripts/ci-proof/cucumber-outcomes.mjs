import { readFileSync } from "node:fs";

const BASE = "website/frontend";
const report = JSON.parse(readFileSync(process.argv[2], "utf8"));

const statusOf = (scenario) => {
  const results = [
    ...(scenario.before ?? []),
    ...(scenario.steps ?? []),
    ...(scenario.after ?? []),
  ].map((item) => item.result?.status);
  if (results.some((status) => status === "failed")) return "failed";
  if (results.length > 0 && results.every((status) => status === "passed")) return "passed";
  return "other";
};

for (const feature of report) {
  for (const element of feature.elements ?? []) {
    if (element.type !== "scenario") continue;
    console.log(`${statusOf(element)}\t${BASE}/${feature.uri}`);
  }
}
