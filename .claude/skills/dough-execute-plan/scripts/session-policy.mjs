// One session's policy: three independent choices a request composes, each
// with its default and the invocation flag that selects its other value.
// Tracking is the ordinary assignment lifecycle or explicitly selected
// one-shot work; workspace is an isolated owned checkout or the default
// checkout; landing waits for review or lands automatically. A flag selects
// only its own choice. Which combinations a workflow supports is that
// workflow's decision, except that non-default workspace and landing
// choices require one-shot tracking. Policy spelling has one owner here.
// No filesystem, Git, or Node-only imports. The `@type {const}` casts let a
// TypeScript caller read each choice's values as literals.

export const sessionPolicyChoices = Object.freeze({
  tracking: Object.freeze({
    values: Object.freeze(/** @type {const} */ (["standard", "one-shot"])),
    flag: "--one-shot",
    option: "oneShot",
  }),
  workspace: Object.freeze({
    values: Object.freeze(
      /** @type {const} */ (["isolated", "default-checkout"]),
    ),
    flag: "--default-main",
    option: "defaultMain",
  }),
  landing: Object.freeze({
    values: Object.freeze(/** @type {const} */ (["review", "auto-land"])),
    flag: "--auto-land",
    option: "autoLand",
  }),
});

// The choices of a session started with no workflow to track, each with its
// default first: it runs in the default checkout unless an isolated workspace
// is chosen, and its landing waits for review unless automatic landing is.
// It has no tracking; a workflow invoked in it later carries its own.
export const unattachedSessionChoices = Object.freeze({
  workspace: Object.freeze({
    values: Object.freeze(
      /** @type {const} */ (["default-checkout", "isolated"]),
    ),
  }),
  landing: Object.freeze({
    values: sessionPolicyChoices.landing.values,
  }),
});

// An unattached session's policy: each choice `selected` names, and the
// default of every choice it does not.
/** @typedef {{workspace: "default-checkout" | "isolated", landing: "review" | "auto-land"}} UnattachedSessionPolicy */
/** @param {Partial<UnattachedSessionPolicy>} [selected]
 * @returns {UnattachedSessionPolicy} */
export function unattachedSessionPolicy(selected = {}) {
  return /** @type {UnattachedSessionPolicy} */ (
    Object.fromEntries(
      Object.entries(unattachedSessionChoices).map(([choice, { values }]) => [
        choice,
        selected[choice] ?? values[0],
      ]),
    )
  );
}

// Each invocation flag and the request option it sets to true.
export const sessionPolicyToggles = Object.freeze(
  Object.fromEntries(
    Object.values(sessionPolicyChoices).map(({ flag, option }) => [
      flag,
      option,
    ]),
  ),
);

// The policy a request's options compose: an option set to true selects its
// choice's second value; anything else keeps the default first value.
/** @typedef {{tracking: "standard" | "one-shot", workspace: "isolated" | "default-checkout", landing: "review" | "auto-land"}} SessionPolicy */
/** @returns {SessionPolicy} */
export function sessionPolicy(options = {}) {
  return /** @type {SessionPolicy} */ (
    Object.fromEntries(
      Object.entries(sessionPolicyChoices).map(
        ([choice, { values, option }]) => [
          choice,
          values[options[option] === true ? 1 : 0],
        ],
      ),
    )
  );
}

// The invocation flags that select a `policy`, in choice order: one for each
// choice that is not its default; none for the default policy.
export function sessionPolicyFlags(policy = {}) {
  return Object.entries(sessionPolicyChoices)
    .filter(([choice, { values }]) => policy[choice] === values[1])
    .map(([, { flag }]) => flag);
}

// The policy the shared start runs with to establish an unattached session's
// place: the one-shot start, which takes no identity and publishes nothing,
// with the session's own workspace and landing. Its flags and publication
// authority are that policy's (`sessionPolicyFlags`,
// `needsPublicationAuthority`); the session itself stays untracked.
/** @param {Partial<UnattachedSessionPolicy>} [selected]
 * @returns {SessionPolicy} */
export function unattachedStartPolicy(selected = {}) {
  return { tracking: "one-shot", ...unattachedSessionPolicy(selected) };
}

// Whether a session's start needs trunk publication authority: tracked work
// publishes its claim, and one-shot work publishes only a result that lands
// automatically.
export function needsPublicationAuthority(policy) {
  return policy.tracking !== "one-shot" || policy.landing === "auto-land";
}

// A successful start `receipt` that also reports a selected automatic
// landing; without that selection its result waits for review.
export function withSelectedLanding(receipt, policy) {
  return receipt.ok && policy.landing === "auto-land"
    ? { ...receipt, landing: policy.landing }
    : receipt;
}

// The selected workspace and landing choices that require one-shot tracking,
// in choice order. Callers spell their refusal with flags or display labels.
/** @param {SessionPolicy} policy
 * @returns {Array<"workspace" | "landing">} */
export function oneShotOnlyChoices(policy) {
  if (policy.tracking === "one-shot") return [];
  return /** @type {Array<"workspace" | "landing">} */ (
    ["workspace", "landing"].filter(
      (choice) => policy[choice] !== sessionPolicyChoices[choice].values[0],
    )
  );
}
