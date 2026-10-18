#!/usr/bin/env node
// Small optional CI command; no scheduler, observer state, or project settings.
import { readFile, writeFile } from "node:fs/promises";
import { pathToFileURL } from "node:url";
import { reusableValidation } from "./ci-reuse-policy.mjs";

export async function runReuseGate(args = process.argv.slice(2)) {
  const inputPath = args[args.indexOf("--input") + 1];
  const outputPath = args[args.indexOf("--output") + 1];
  let decision;
  try {
    let input = "";
    if (args.includes("--input")) input = await readFile(inputPath, "utf8");
    else for await (const chunk of process.stdin) input += chunk;
    decision = reusableValidation(JSON.parse(input));
  } catch {
    decision = { reuse: false, reason: "unreadable-evidence" };
  }
  const report = `${JSON.stringify(decision)}\n`;
  if (args.includes("--output")) await writeFile(outputPath, report);
  else process.stdout.write(report);
  return decision;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href)
  await runReuseGate();
