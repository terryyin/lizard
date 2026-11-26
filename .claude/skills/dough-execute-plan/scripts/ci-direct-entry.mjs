import { realpathSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";

// Run the CLI only when its module is the invoked script. With a platform
// path alias such as macOS `/tmp` for `/private/tmp`, Node canonicalizes the
// module path while argv keeps the caller's spelling. Compare real paths
// so either spelling of the same file invokes the CLI.
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
