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
// The Cursor generation managed delivery reports when the coordinator's own
// generation is not supplied; the owner's next real hook replaces it.
export const managedDeliveryGeneration = "managed-delivery";

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

// The identity of the checkout that armed a mailbox: the one recorded when it
// was armed, whatever its path has become since, so the repository's other
// worktrees still reach the observer after that worktree is removed and
// another repository created at its path does not. A mailbox that recorded
// none is identified by the checkout now at its path.
function armingIdentity(request) {
  return request.identity ?? checkoutIdentity(request.root);
}

// `root` is the checkout reading the mailbox.
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
  if (checkoutIdentity(root) !== armingIdentity(request))
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
    JSON.stringify({ ...request, root, identity: checkoutIdentity(root) }),
    { mode: 0o600 },
  );
  mkdirSync(join(directory, "events"), { mode: 0o700 });
  return directory;
}
