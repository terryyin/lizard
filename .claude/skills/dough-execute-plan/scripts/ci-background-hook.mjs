// Native asyncRewake Stop hook: local mailbox reads only, one attempted event.
import {
  readBackgroundHandoff,
  saveBackgroundHandoff,
} from "./ci-background-state.mjs";
import { waitForBackgroundFailure } from "./ci-background-waiter.mjs";
import { isDirectCliEntry } from "./ci-direct-entry.mjs";

export { waitForBackgroundFailure } from "./ci-background-waiter.mjs";

if (isDirectCliEntry(import.meta.url, process.argv[1])) {
  try {
    let raw = "";
    for await (const chunk of process.stdin) raw += chunk;
    const feedback = await waitForBackgroundFailure(JSON.parse(raw));
    if (feedback) {
      await new Promise((resolve, reject) =>
        process.stderr.write(`${feedback.message}\n`, (error) =>
          error ? reject(error) : resolve(),
        ),
      );
      const handoff = readBackgroundHandoff(feedback.directory);
      saveBackgroundHandoff(feedback.directory, {
        ...handoff,
        feedbackEmitted: true,
      });
      process.exitCode = 2;
    }
  } catch (error) {
    // A hook error is not a CI failure and must not generate a wake.
    process.stderr.write(
      `CI background waiter unavailable: ${String(error).slice(0, 600)}\n`,
    );
    process.exitCode = 1;
  }
}
