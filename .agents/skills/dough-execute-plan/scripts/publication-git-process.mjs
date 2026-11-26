// Local Git subprocesses and the bounded process group for remote transport.
import { execFile, spawn } from "node:child_process";
import { promisify } from "node:util";
import { gitExecutable } from "./git-executable.mjs";

export const exec = promisify(execFile);

// `exec` with `input` as standard input. A command that exits before reading
// all of it, as Git does when a hook rejects a commit, fails through its exit
// status instead of an unhandled EPIPE on the closed pipe.
export function execWithInput(command, args, options, input) {
  const running = exec(command, args, options);
  running.child.stdin.on("error", (error) => {
    if (error.code !== "EPIPE") throw error;
  });
  running.child.stdin.end(input);
  return running;
}

// Remote transport commands answer within a bound; local commands do not need
// one. The default suits ordinary networks; the setting raises or lowers it.
const transportSubcommands = new Set(["fetch", "push", "ls-remote"]);
export const transportBoundSetting = "OPEN_DOUGH_GIT_TRANSPORT_BOUND_MS";
const defaultTransportBoundMs = 120_000;
const terminationGraceMs = 1_000;

// The bound in force now: the setting when it is a positive integer.
export function transportBoundMs(env = process.env) {
  const value = env[transportBoundSetting] ?? "";
  return /^[1-9]\d*$/.test(value) ? Number(value) : defaultTransportBoundMs;
}

// A remote transport command's subcommand and remote (its first operand);
// null for local commands. Managed callers name the subcommand first.
function transportOf([subcommand, ...operands]) {
  if (!transportSubcommands.has(subcommand)) return null;
  const remote = operands.find((arg) => !arg.startsWith("-")) ?? null;
  return { subcommand, remote };
}

// A trailing `{ signal }` lets a transport command end with its caller.
export async function git(cwd, ...args) {
  const { signal } = typeof args.at(-1) === "object" ? args.pop() : {};
  const transport = transportOf(args);
  const executable = await gitExecutable({ cwd });
  if (transport)
    return boundedTransport(executable, cwd, args, transport, signal);
  return exec(executable, args, { cwd });
}

function signalProcessGroup(pid, signal) {
  try {
    process.kill(-pid, signal);
  } catch {
    // The group already ended.
  }
}

// Runs one remote transport command in its own process group and resolves as
// `exec` does. When the bound expires or `signal` aborts, the whole group (Git
// and the transport it started, such as ssh) receives SIGTERM, then SIGKILL
// once it closes or a short grace passes. Expiry rejects with
// `code: "transport-timeout"`, an abort with the signal's reason, and other
// failures with the shape `exec` gives a failed command.
/** @returns {Promise<{ stdout: string, stderr: string }>} */
function boundedTransport(
  executable,
  cwd,
  args,
  { subcommand, remote },
  signal,
) {
  const boundMs = transportBoundMs();
  const cmd = `git ${args.join(" ")}`;
  return new Promise((resolve, reject) => {
    if (signal?.aborted) return reject(signal.reason);
    const child = spawn(executable, args, {
      cwd,
      detached: true,
      stdio: ["ignore", "pipe", "pipe"],
    });
    const bound = AbortSignal.timeout(boundMs);
    const ending = signal ? AbortSignal.any([signal, bound]) : bound;
    const output = { stdout: "", stderr: "" };
    for (const stream of ["stdout", "stderr"]) {
      child[stream].setEncoding("utf8").on("data", (chunk) => {
        output[stream] += chunk;
      });
    }
    const fail = (error, fields) =>
      reject(Object.assign(error, fields, { cmd, ...output }));
    const end = () => {
      signalProcessGroup(child.pid, "SIGTERM");
      const stop = () => {
        clearTimeout(grace);
        child.off("close", stop);
        signalProcessGroup(child.pid, "SIGKILL");
        if (ending.reason !== bound.reason) return reject(ending.reason);
        const error = new Error(
          `${cmd} did not answer within ${boundMs} ms; its process tree was stopped`,
        );
        const facts = { subcommand, remote, boundMs, killed: true };
        fail(error, { code: "transport-timeout", ...facts });
      };
      const grace = setTimeout(stop, terminationGraceMs);
      child.once("close", stop);
    };
    ending.addEventListener("abort", end, { once: true });
    child.on("error", (error) => {
      ending.removeEventListener("abort", end);
      fail(error, {});
    });
    child.on("close", (code, exitSignal) => {
      if (ending.aborted) return;
      ending.removeEventListener("abort", end);
      if (code === 0) resolve(output);
      else {
        const error = new Error(`Command failed: ${cmd}\n${output.stderr}`);
        fail(error, { code, killed: false, signal: exitSignal });
      }
    });
  });
}
