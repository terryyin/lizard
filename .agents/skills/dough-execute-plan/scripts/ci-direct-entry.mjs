import { realpathSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";

// A checkout-bound CLI script is invoked as `node <argv path>`. It should run
// its CLI body only when it is the directly executed entry module, not when
// another module imports it. The straightforward check compares
// `import.meta.url` with `pathToFileURL(argv path)`, but a script invoked
// through a symlink-equivalent spelling (for example a project's symlinked
// `.claude/skills/<name>` directory, or macOS mounting `/tmp` as
// `/private/tmp`) resolves its module path to the canonical file while argv
// keeps the spelling the invoker used. Those two URLs then differ even though
// they name the same file, so the literal comparison alone misses direct
// invocations that reach the script through such a path. Recognize that case
// by canonical filesystem identity, without weakening the case where the
// paths genuinely differ.
export function isDirectCliEntry(moduleUrl, argvPath) {
  if (!argvPath) return false;
  if (moduleUrl === pathToFileURL(argvPath).href) return true;
  try {
    return realpathSync(fileURLToPath(moduleUrl)) === realpathSync(argvPath);
  } catch {
    return false;
  }
}

// Reads the `--flag value` pairs after a command name into camel-cased keys.
// Every flag carries a value; a flag in a value's place is unknown.
export function flagValues(argv, defaults = {}) {
  const result = { ...defaults };
  for (let index = 0; index < argv.length; index += 2) {
    const flag = argv[index];
    if (
      !flag.startsWith("--") ||
      index + 1 >= argv.length ||
      argv[index + 1].startsWith("--")
    ) {
      throw new Error(`invalid argument ${flag}`);
    }
    const key = flag
      .slice(2)
      .replace(/-[a-z]/g, (match) => match[1].toUpperCase());
    result[key] = argv[index + 1];
  }
  return result;
}
