import { execFileSync } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  writeFileSync,
} from "node:fs";
import { basename, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const checkoutRoot = fileURLToPath(
  new URL("../../../../", import.meta.url),
);
// Native hooks and Nix launchers share this directory; it must not follow TMPDIR.
export const mailboxRoot =
  process.env.DOUGH_CI_MAILBOX_ROOT ??
  join("/tmp", `dough-ci-${process.getuid?.() ?? "user"}`);
export const receiptPrefix = "CI_OBSERVER ";

function gitCommonDir(root) {
  try {
    const checkout = realpathSync(root);
    const [toplevel, common] = execFileSync(
      "git",
      ["-C", checkout, "rev-parse", "--show-toplevel", "--git-common-dir"],
      { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] },
    )
      .trim()
      .split("\n");
    if (realpathSync(toplevel) !== checkout) return;
    return realpathSync(resolve(checkout, common));
  } catch {
    return;
  }
}

// A checkout that is its Git toplevel is identified by its repository, so
// worktrees of one repository share an identity; any other root is itself.
export function checkoutIdentity(root) {
  return gitCommonDir(root) ?? resolve(root);
}

export function readMailbox(
  directory,
  root = checkoutRoot,
  storage = mailboxRoot,
) {
  if (
    resolve(directory, "..") !== resolve(storage) ||
    !/^watch-/.test(basename(directory))
  ) {
    throw new Error("CI mailbox is outside the observer directory");
  }
  const request = JSON.parse(
    readFileSync(join(directory, "request.json"), "utf8"),
  );
  if (checkoutIdentity(request.root) !== checkoutIdentity(root))
    throw new Error("CI mailbox belongs to another checkout");
  return request;
}

export function createMailbox(
  request,
  { root = checkoutRoot, storage = mailboxRoot } = {},
) {
  mkdirSync(storage, { recursive: true, mode: 0o700 });
  const directory = mkdtempSync(join(storage, "watch-"));
  writeFileSync(
    join(directory, "request.json"),
    JSON.stringify({ ...request, root }),
    { mode: 0o600 },
  );
  mkdirSync(join(directory, "events"), { mode: 0o700 });
  return directory;
}
