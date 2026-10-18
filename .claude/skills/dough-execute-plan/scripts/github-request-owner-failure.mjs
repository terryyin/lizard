// A request whose local owner provided no usable reply. Before connecting,
// it was unasked; after sending, the reader cannot infer upstream activity.
// Only these observed categories leave the endpoint/link; arbitrary errors,
// caller tokens, login credentials and endpoint paths never do.

const explanations = {
  "not-directory": "its endpoint is not a directory",
  "another-user": "its endpoint belongs to another user",
  "not-private": "its endpoint is open to other users",
  "path-too-long": "its endpoint path is too long for a local socket",
  "start-failed": "its process could not start",
  "connection-failed": "its local connection failed",
  incompatible: "it explicitly refused the protocol",
  unauthenticated: "it explicitly refused this caller",
  unknown: "it gave no usable answer",
};

/** @typedef {keyof typeof explanations} OwnerUnavailableCause */
/** @typedef {{ readonly kind: "owner-unavailable"; readonly cause: OwnerUnavailableCause }} OwnerUnavailableFailure */

// Unknown exceptions remain unknown; never classify by arbitrary error text.
export class OwnerUnavailable extends Error {
  /** @param {OwnerUnavailableCause} cause */
  constructor(cause) {
    super(ownerFailureText(cause));
    /** @readonly @type {OwnerUnavailableFailure} */
    this.failure = { kind: "owner-unavailable", cause: safeOwnerCause(cause) };
  }
}

/** @param {unknown} error @returns {OwnerUnavailableFailure} */
export function ownerFailure(error) {
  return error instanceof OwnerUnavailable
    ? error.failure
    : { kind: "owner-unavailable", cause: "unknown" };
}

/** @param {unknown} cause @returns {OwnerUnavailableCause} */
export function safeOwnerCause(cause) {
  return typeof cause === "string" && Object.hasOwn(explanations, cause)
    ? /** @type {OwnerUnavailableCause} */ (cause)
    : "unknown";
}

/** @param {OwnerUnavailableCause} cause */
export function ownerFailureText(cause) {
  const observed = explanations[safeOwnerCause(cause)];
  return `The local GitHub request owner is unavailable: ${observed}. No GitHub answer is available. Check the local request-owner endpoint configuration and restart the reader.`;
}

// Directory metadata supplies the observation even when a test cannot create
// a directory owned by another OS user.
export function refusedPrivacy(refusal) {
  return new OwnerUnavailable(
    {
      "not a directory": "not-directory",
      "owned by another user": "another-user",
      "open to other users": "not-private",
    }[refusal] ?? "unknown",
  );
}
