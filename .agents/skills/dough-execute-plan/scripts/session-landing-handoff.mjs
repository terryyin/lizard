import { execFileSync } from "node:child_process";
import { readFileSync, realpathSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { formatEstablishedSession } from "./established-session.mjs";
import { managementContext } from "./publication-git.mjs";

const identities = {
  claude: "CLAUDE_CODE_SESSION_ID",
  codex: "CODEX_THREAD_ID",
};

function nativeShell() {
  const found = Object.entries(identities).filter(
    ([, key]) => process.env[key],
  );
  return found.length === 1
    ? [found[0][0], process.env[found[0][1]], process.cwd()]
    : [];
}

function cursorProbe(input) {
  if (
    !input.cursor_version ||
    input.hook_event_name !== "postToolUse" ||
    input.tool_name !== "Shell" ||
    input.tool_input?.command !==
      "node .agents/skills/dough-execute-plan/scripts/session-landing-handoff.mjs"
  )
    return [];
  return ["cursor", input.conversation_id, input.cwd];
}

async function retainedHandoff(host, id, cwd) {
  if (typeof id !== "string" || !id || typeof cwd !== "string" || !cwd) return;
  try {
    const git = (...args) =>
      execFileSync("git", args, {
        cwd,
        encoding: "utf8",
        stdio: ["ignore", "pipe", "ignore"],
      }).trimEnd();
    const workspace = realpathSync(git("rev-parse", "--show-toplevel"));
    const branch = git("symbolic-ref", "--short", "HEAD");
    const records = JSON.parse(
      readFileSync(
        join(homedir(), ".open-dough/dashboard/agent-launches.json"),
        "utf8",
      ),
    );
    const matches = Object.values(records)
      .flat()
      .filter(
        (entry) =>
          entry?.session?.host === host && entry.session.sessionId === id,
      );
    if (matches.length !== 1) return;
    const {
      request,
      session,
      establishedSession: established,
      landingReporting,
      doneAt,
    } = matches[0];
    const { authority, settledAt, deletedAt } = landingReporting ?? {};
    const { source, reporting } = request;
    if (
      doneAt ||
      deletedAt ||
      !settledAt ||
      request.host !== host ||
      request.workflow !== "ad-hoc" ||
      request.instruction?.trim() ||
      request.attachments?.length ||
      request.followUp ||
      request.policy?.landing !== "auto-land" ||
      established?.landing !== "auto-land" ||
      !request.storyIdentity ||
      request.storyIdentity !== authority?.identity ||
      !source ||
      !reporting?.reference ||
      !reporting.command ||
      !reporting.landingContext ||
      !reporting.origin ||
      reporting.origin !== landingReporting.origin ||
      request.policy.workspace !== established.role ||
      established.branch !== branch ||
      authority.branch !== branch ||
      established.remote !== authority.remote ||
      `refs/heads/${established.target}` !== authority.target ||
      realpathSync(established.workspace) !== workspace ||
      realpathSync(authority.workspace) !== workspace ||
      (session.continuation &&
        realpathSync(session.continuation.workspace) !== workspace) ||
      realpathSync(await managementContext(workspace)) !==
        realpathSync(authority.repository)
    )
      return;
    return [
      formatEstablishedSession(established),
      `Taken story: ${request.storyIdentity}`,
      `Landing context: ${reporting.landingContext}`,
      [
        "Dashboard reporting context:",
        `- project: ${source}`,
        `- host: ${host}`,
        `- launch reference: ${reporting.reference}`,
        `- reporting command: ${reporting.command}`,
      ].join("\n"),
    ].join("\n\n");
  } catch {
    return undefined;
  }
}

const args = process.argv.slice(2);
if (args.length === 0) {
  const handoff = await retainedHandoff(...nativeShell());
  if (handoff) process.stdout.write(`${handoff}\n`);
} else if (args.length === 1 && args[0] === "--cursor-hook") {
  try {
    const handoff = await retainedHandoff(
      ...cursorProbe(JSON.parse(readFileSync(0, "utf8"))),
    );
    process.stdout.write(
      `${JSON.stringify(handoff ? { additional_context: handoff } : {})}\n`,
    );
  } catch {
    process.stdout.write("{}\n");
  }
}
