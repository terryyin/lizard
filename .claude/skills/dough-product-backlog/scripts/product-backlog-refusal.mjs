// How this tool says no. Every refusal is a BacklogError carrying the same
// promise — the file was left as it was — and every missing input is refused
// in the one shape below, so a caller reads the same kind of answer whichever
// operation they asked for.

export class BacklogError extends Error {
  constructor(message) {
    super(message);
    this.name = "BacklogError";
  }

  // Every refusal carries the same promise: the file was left as it was.
  get refusal() {
    return `${this.message}\nThe backlog was not changed.`;
  }
}

// The one shape of a missing-input refusal. Each operation supplies the hint
// that suits it, because what to do about a missing value differs between
// writing a new entry and naming an entry the backlog already carries.
export function requireField(value, field, hint = "") {
  if (typeof value !== "string" || value.trim() === "") {
    throw new BacklogError(`Missing ${field}: supply --${field}.${hint}`);
  }
}
