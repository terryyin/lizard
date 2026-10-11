// The private endpoint of one GitHub request owner
// (`./github-request-owner.mjs`): where it stands and who may reach it; what
// is said there is `./github-request-owner-wire.mjs`'s. It stands in a
// directory only this user can enter, under `/tmp/dough-github-requests-UID/`
// unless `DOUGH_GITHUB_REQUEST_OWNER_ROOT` names another -- beside the CI
// mailboxes (`./ci-mailbox-location.mjs`), where every host that can run an
// observer can write, and short enough for a socket path -- one directory
// for each API host and login `gh` asks with, however a process comes by
// that login, so each has one owner and no consumer is answered with
// another's login. The owner standing there is the one its highest-numbered
// record names; a record is claimed once and never reused.
import { execFile } from "node:child_process";
import { createHmac, randomBytes } from "node:crypto";
import {
  linkSync,
  lstatSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

function endpointRoot(env) {
  return (
    env.DOUGH_GITHUB_REQUEST_OWNER_ROOT ??
    join("/tmp", `dough-github-requests-${process.getuid?.() ?? "user"}`)
  );
}

// Why a directory cannot hold the endpoint for the user `uid`; undefined when
// it is that user's alone.
/**
 * @param {{ isDirectory(): boolean; readonly uid: number; readonly mode: number }} entry
 * @param {number | undefined} uid
 */
export function privacyRefusal(entry, uid) {
  if (!entry.isDirectory()) return "not a directory";
  if (entry.uid !== uid) return "owned by another user";
  if ((entry.mode & 0o077) !== 0) return "open to other users";
  return undefined;
}

function privateDirectory(path) {
  mkdirSync(path, { recursive: true, mode: 0o700 });
  const refusal = privacyRefusal(lstatSync(path), process.getuid?.());
  if (refusal !== undefined)
    throw new Error(`GitHub request endpoint is ${refusal}`);
  return path;
}

// Writes `text` at `file` unless a file already stands there: complete for
// every reader, and written by one writer only. Whether this write stands.
function writtenOnce(file, text) {
  const draft = `${file}.${process.pid}.${randomBytes(4).toString("hex")}`;
  writeFileSync(draft, text, { mode: 0o600, flag: "wx" });
  try {
    linkSync(draft, file);
    return true;
  } catch (error) {
    if (error.code !== "EEXIST") throw error;
    return false;
  } finally {
    rmSync(draft, { force: true });
  }
}

// This user's secret under `root`, which names endpoint directories without
// keeping any login in a name.
function namingSecret(root) {
  const file = join(root, "key");
  try {
    return readFileSync(file, "utf8");
  } catch {
    writtenOnce(file, randomBytes(32).toString("hex"));
    return readFileSync(file, "utf8");
  }
}

// Everything a change of host or login shows in, read without asking `gh`:
// each setting named for `gh` or GitHub, where this endpoint's root is, and
// the state of the file `gh` stores its logins in.
function loginSignature(env) {
  const settings = Object.entries(env).filter(([name]) =>
    /^(GH|GITHUB|DOUGH_GITHUB)_/.test(name),
  );
  const configured =
    env.GH_CONFIG_DIR ||
    join(env.XDG_CONFIG_HOME || join(env.HOME || homedir(), ".config"), "gh");
  let stored = null;
  try {
    const { mtimeMs, ctimeMs, size, ino } = statSync(
      join(configured, "hosts.yml"),
    );
    stored = [configured, mtimeMs, ctimeMs, size, ino];
  } catch {
    // No stored login.
  }
  return JSON.stringify([settings, stored]);
}

// How long `gh` may take to name its login before a request goes unanswered.
const loginLimitMs = 10_000;

// The token `gh` would ask its host with now -- from a token variable or its
// stored logins, as `gh` itself decides -- read on this machine, never asked
// of GitHub; empty without a login. It names the endpoint directory only
// through this user's secret and is not kept, written or said. Rejects when
// `gh` gave no answer in time.
function loginToken(env) {
  return new Promise((resolve, reject) => {
    execFile(
      "gh",
      ["auth", "token"],
      {
        env: { ...env, GH_PROMPT_DISABLED: "1" },
        timeout: loginLimitMs,
        encoding: "utf8",
      },
      (error, stdout) => {
        if (error?.killed) reject(new Error("gh did not name its login"));
        else resolve(error ? "" : stdout.trim());
      },
    );
  });
}

// The longest path a socket may be bound at on every supported system.
const socketPathBytes = 103;

// The directory holding the endpoint for the API host and login a process
// with `env` asks with now, created private when missing; throws when it is
// not this user's alone or lies too deep for a socket. The host is the one
// `GH_HOST` names, `github.com` otherwise.
async function endpointDirectory(env) {
  const root = privateDirectory(endpointRoot(env));
  const host = (env.GH_HOST || "github.com").toLowerCase();
  const name = createHmac("sha256", namingSecret(root))
    .update(JSON.stringify([host, await loginToken(env)]))
    .digest("hex")
    .slice(0, 12);
  const directory = privateDirectory(join(root, name));
  if (Buffer.byteLength(join(directory, newSocketName())) > socketPathBytes)
    throw new Error("GitHub request endpoint is too deep for a socket path");
  return directory;
}

// The endpoint a process asks through, established once and again whenever
// its host or login may have changed.
export class EndpointBinding {
  #env;

  /** @type {{ readonly signature: string; readonly directory: Promise<string> } | undefined} */
  #established;

  constructor(env = process.env) {
    this.#env = env;
  }

  // The endpoint directory for the host and login `gh` asks with now. A
  // directory that could not be established is established again when next
  // asked for.
  /** @returns {Promise<string>} */
  directory() {
    const signature = loginSignature(this.#env);
    if (this.#established?.signature !== signature) {
      const established = {
        signature,
        directory: endpointDirectory(this.#env),
      };
      this.#established = established;
      established.directory.catch(() => {
        if (this.#established === established) this.#established = undefined;
      });
    }
    return this.#established.directory;
  }
}

const recordName = /^owner\.(\d+)\.json$/;
const socketName = /^[0-9a-f]{8}\.s$/;
const recordFile = (directory, generation) =>
  join(directory, `owner.${generation}.json`);

export const newSocketName = () => `${randomBytes(4).toString("hex")}.s`;

function generations(directory) {
  return readdirSync(directory)
    .map((name) => Number(recordName.exec(name)?.[1]))
    .filter(Number.isSafeInteger)
    .sort((a, b) => a - b);
}

// The record of the owner standing in `directory`: its generation and, when
// readable, its process, protocol, caller token and socket path.
/**
 * @returns {{
 *   readonly generation: number;
 *   readonly pid?: number;
 *   readonly protocol?: number;
 *   readonly token?: string;
 *   readonly socket?: string;
 * } | undefined}
 */
export function standingRecord(directory) {
  const generation = generations(directory).at(-1);
  if (generation === undefined) return undefined;
  try {
    const {
      pid,
      protocol: spoken,
      token,
      socket,
    } = JSON.parse(readFileSync(recordFile(directory, generation), "utf8"));
    return {
      generation,
      pid,
      protocol: spoken,
      token,
      ...(socketName.test(socket) ? { socket: join(directory, socket) } : {}),
    };
  } catch {
    return { generation };
  }
}

// Claims `generation` for the owner `record` describes, unless it was
// claimed already. The claimant then stands and removes what earlier owners
// left.
export function claimedRecord(directory, generation, record) {
  if (!writtenOnce(recordFile(directory, generation), JSON.stringify(record)))
    return false;
  for (const earlier of generations(directory)) {
    if (earlier >= generation) break;
    const file = recordFile(directory, earlier);
    try {
      const { socket } = JSON.parse(readFileSync(file, "utf8"));
      if (socketName.test(socket)) rmSync(join(directory, socket));
    } catch {
      // Nothing of that owner's is left to remove.
    }
    rmSync(file, { force: true });
  }
  return true;
}
