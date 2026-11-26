// The executable a Git call starts. On macOS, Apple's `/usr/bin/git` is a
// launcher that asks `xcrun` for the developer-tool Git on every call; when
// the call's PATH selects that launcher, the developer-tool Git it would run
// starts directly instead. A Git earlier on PATH, Linux, and an unresolved
// developer tool keep the ordinary `git` invocation. PATH itself is untouched.
import { execFile } from "node:child_process";
import { access, constants } from "node:fs/promises";
import path from "node:path";

const appleLauncher = "/usr/bin/git";
const developerGits = new Map();

/**
 * `env` is the call's whole environment; `cwd` anchors relative PATH entries.
 * @param {{ env?: NodeJS.ProcessEnv, cwd?: string, platform?: NodeJS.Platform }} [call]
 * @returns {Promise<string>}
 */
export async function gitExecutable({
  env = process.env,
  cwd = process.cwd(),
  platform = process.platform,
} = {}) {
  if (platform !== "darwin") return "git";
  if ((await firstOnPath("git", env.PATH ?? "", cwd)) !== appleLauncher)
    return "git";
  return (await developerGit(env)) ?? "git";
}

async function firstOnPath(name, searchPath, cwd) {
  for (const entry of searchPath.split(path.delimiter)) {
    const candidate = path.resolve(cwd, entry, name);
    try {
      await access(candidate, constants.X_OK);
      return candidate;
    } catch {
      // Not here; keep looking.
    }
  }
  return undefined;
}

// One `xcrun` answer per developer directory, including a failed one.
function developerGit(env) {
  const key = env.DEVELOPER_DIR ?? "";
  let found = developerGits.get(key);
  if (found === undefined) {
    found = new Promise((resolve) => {
      execFile(
        "/usr/bin/xcrun",
        ["-f", "git"],
        { encoding: "utf8", env },
        (error, stdout) => {
          const selected = stdout.trim();
          resolve(
            error || !path.isAbsolute(selected) || selected === appleLauncher
              ? undefined
              : selected,
          );
        },
      );
    });
    developerGits.set(key, found);
  }
  return found;
}
