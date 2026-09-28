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
