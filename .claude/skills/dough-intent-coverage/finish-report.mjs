import { readFileSync } from "node:fs";
import { stdout } from "node:process";

const body = readFileSync(0, "utf8");
const startArgument = process.argv[2] ?? "";
const startMs = Number(startArgument);

await new Promise((resolve, reject) => {
  stdout.write(body, (error) => {
    if (error) reject(error);
    else resolve();
  });
});

const endMs = Date.now();
const hasStart =
  /^\d+$/.test(startArgument) &&
  Number.isSafeInteger(startMs) &&
  startMs > 0 &&
  startMs <= endMs;
const elapsed = hasStart
  ? `${endMs - startMs} ms (measured), from ${startMs} to ${endMs} in Unix epoch ms. The end clock was sampled after writing the prepared report body.`
  : "Unavailable. The invocation start was missing, invalid, or later than the report end clock.";

stdout.write(`\n\n- **Invocation-to-report elapsed.** ${elapsed}\n`);
