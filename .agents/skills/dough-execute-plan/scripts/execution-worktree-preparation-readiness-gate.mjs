import { execFile } from "node:child_process";
import { existsSync, lstatSync, readFileSync, realpathSync } from "node:fs";
import { join, sep } from "node:path";
import { promisify } from "node:util";

const exec = promisify(execFile);

export function readProjectConvention(checkout) {
  const contributing = join(checkout, "CONTRIBUTING.md");
  if (!existsSync(contributing)) {
    return { missing: true, reason: "CONTRIBUTING.md is missing" };
  }
  const text = readFileSync(contributing, "utf8");
  const setup = text.match(/Locked setup:\s*`([^`]+)`/);
  const command = text.match(/Applicable command:\s*`([^`]+)`/);
  if (!setup || !command) {
    return {
      missing: true,
      reason: "setup or applicable command is ambiguous",
    };
  }
  return { setup: setup[1], command: command[1] };
}

async function runCommand(cwd, commandLine, env) {
  const [command, ...args] = commandLine.split(/\s+/).filter(Boolean);
  try {
    const result = await exec(command, args, {
      cwd,
      env,
      timeout: 60_000,
      maxBuffer: 2_000_000,
    });
    return {
      command: commandLine,
      cwd,
      code: 0,
      stdout: result.stdout,
      stderr: result.stderr,
    };
  } catch (error) {
    const code =
      typeof error.code === "number"
        ? error.code
        : error.code === "ERR_SOCKET_TIMEOUT"
          ? -1
          : 1;
    return {
      command: commandLine,
      cwd,
      code,
      stdout: error.stdout ?? "",
      stderr: error.stderr ?? String(error.message ?? error),
    };
  }
}

function failureReport(executionCheckout, command, failure) {
  return [
    `Failed to prepare execution checkout ${executionCheckout}`,
    `Command: ${command}`,
    failure,
  ].join("\n");
}

function sameCheckout(left, right) {
  try {
    return realpathSync(left) === realpathSync(right);
  } catch {
    return false;
  }
}

function hostEvidenceMatches(execution, hostPreparation, currentState) {
  if (!hostPreparation || currentState === undefined || currentState === null) {
    return false;
  }
  if (
    hostPreparation.dependencyState === undefined ||
    hostPreparation.dependencyState === null
  ) {
    return false;
  }
  if (hostPreparation.dependencyState !== currentState) return false;
  return sameCheckout(hostPreparation.checkout, execution);
}

// Ownership of mutable install in the selected checkout, not an availability
// test and not parent-directory or symlink reuse evidence.
function checkoutOwnsMutableInstall(checkout) {
  const installed = join(checkout, "node_modules");
  if (!existsSync(installed)) return false;
  try {
    if (lstatSync(installed).isSymbolicLink()) return false;
    const realInstalled = realpathSync(installed);
    const realCheckout = realpathSync(checkout);
    return (
      realInstalled === realCheckout ||
      realInstalled.startsWith(realCheckout + sep)
    );
  } catch {
    return false;
  }
}

function failedResult(execution, convention, invocations, command, failure) {
  return {
    ok: false,
    reused: false,
    convention,
    invocations,
    report: failureReport(execution, command, failure),
  };
}

function delegatedResult(execution, convention, invocations, reused) {
  invocations.push({
    role: "delegate",
    command: "implementation",
    cwd: execution,
    code: 0,
    stdout: "",
    stderr: "",
  });
  return { ok: true, reused, convention, invocations, report: null };
}

async function runOrdinaryReadiness(execution, env, convention, invocations) {
  const setup = await runCommand(execution, convention.setup, env);
  invocations.push({ role: "setup", ...setup });
  if (setup.code !== 0) {
    return failedResult(
      execution,
      convention,
      invocations,
      convention.setup,
      setup.stderr || `exit ${setup.code}`,
    );
  }

  const command = await runCommand(execution, convention.command, env);
  invocations.push({ role: "command", ...command });
  if (command.code !== 0) {
    return failedResult(
      execution,
      convention,
      invocations,
      convention.command,
      command.stderr || `exit ${command.code}`,
    );
  }

  return delegatedResult(execution, convention, invocations, false);
}

// Cheap substitute actor: follow the fixture's project-owned convention,
// reuse host-established preparation only for this checkout and dependency
// state when the command succeeds there, otherwise record setup then command,
// and delegate only after the command succeeds. Not product runtime.
export async function runReadinessGate(execution, env, options = {}) {
  const invocations = [];
  const convention = readProjectConvention(execution);
  if (convention.missing) {
    return failedResult(
      execution,
      convention,
      invocations,
      "missing",
      convention.reason,
    );
  }

  const currentState =
    typeof options.dependencyStateOf === "function"
      ? options.dependencyStateOf(execution)
      : undefined;
  const reusable =
    hostEvidenceMatches(execution, options.hostPreparation, currentState) &&
    checkoutOwnsMutableInstall(execution);

  if (reusable) {
    const command = await runCommand(execution, convention.command, env);
    if (command.code === 0) {
      invocations.push({ role: "command", ...command });
      return delegatedResult(execution, convention, invocations, true);
    }
  }

  return runOrdinaryReadiness(execution, env, convention, invocations);
}
