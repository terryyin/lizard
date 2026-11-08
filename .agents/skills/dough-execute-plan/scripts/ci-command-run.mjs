// One CI adapter invocation (`./ci-command-adapter.mjs`): the configured
// command run in its folder with one JSON request on stdin and one JSON
// answer on stdout, bounded in time and, per call, in answer size.
import { execFile } from "node:child_process";

export const defaultAdapterTimeoutMs = 20_000;
export const adapterResponseBytes = 64 * 1024;

// Answers the adapter's parsed JSON answer; a failed run rejects with the
// child-process error, which keeps what the adapter wrote to stderr.
// `answerBytes` is how much stdout this call may keep (default 64 KiB);
// only the dashboard's proof read raises it to the collector contract's
// summary read limit.
export function runAdapter(
  command,
  request,
  {
    signal,
    cwd,
    timeoutMs = defaultAdapterTimeoutMs,
    answerBytes = adapterResponseBytes,
  },
) {
  return new Promise((resolve, reject) => {
    const child = execFile(
      command[0],
      command.slice(1),
      {
        cwd,
        signal,
        timeout: timeoutMs,
        maxBuffer: answerBytes,
        encoding: "utf8",
      },
      (error, stdout, stderr) => {
        if (error) return reject(Object.assign(error, { stderr }));
        try {
          resolve(JSON.parse(stdout));
        } catch (parseError) {
          reject(
            new Error(
              `CI adapter returned invalid JSON: ${parseError.message}`,
            ),
          );
        }
      },
    );
    child.stdin.end(`${JSON.stringify(request)}\n`);
  });
}

export function runAdapterForAttempt(
  command,
  operation,
  check,
  { runId, attemptId, sha },
  options,
) {
  return runAdapter(
    command,
    { operation, check, attempt: { runId, attemptId, sha } },
    options,
  );
}

// `value` cut to at most `limit` UTF-8 bytes on a character boundary.
export function truncateUtf8(value, limit) {
  let text = "";
  let bytes = 0;
  for (const character of value) {
    const size = Buffer.byteLength(character);
    if (bytes + size > limit) return { text, truncated: true };
    text += character;
    bytes += size;
  }
  return { text, truncated: false };
}
