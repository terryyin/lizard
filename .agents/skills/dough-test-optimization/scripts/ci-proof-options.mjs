// The collector command line's options (ci-proof-summary.mjs): a
// command's string options and flags, positive integers, the attempt they
// name, and the units `--unit` and `--expect <entry>=<parts>[:<unit>]`
// declare, each refused naming the option it came from. Node built-ins
// only, like the rest of the collector.
import { parseArgs } from "node:util";
import {
  DECLARED_UNITS,
  attemptProblems,
  isPositive,
} from "./ci-proof-contract.mjs";

// The positive integer TEXT gives option NAME.
export function positive(name, text) {
  const value = Number(text);
  if (!/^[0-9]+$/.test(text ?? "") || !isPositive(value)) {
    throw new Error(`--${name} ${text} is not a positive integer`);
  }
  return value;
}

// A command's options: strings, every REQUIRED one must be given, and each
// of MULTIPLE may be given more than once; each of FLAGS is a boolean.
export function options(
  argv,
  { required, optional = [], multiple = [], flags = [] },
) {
  const names = [...required, ...optional];
  const { values } = parseArgs({
    args: argv,
    options: Object.fromEntries([
      ...names.map((name) => [
        name,
        { type: "string", multiple: multiple.includes(name) },
      ]),
      ...flags.map((name) => [name, { type: "boolean" }]),
    ]),
  });
  for (const name of required) {
    if (values[name] === undefined || values[name] === "") {
      throw new Error(`--${name} is required`);
    }
  }
  return values;
}

// The attempt that VALUES name for a COMMAND: `--attempt-id`, or the
// integer options GITHUB maps to their identity fields.
export function attemptIdentity(command, values, github) {
  const identity = {};
  for (const [name, field] of Object.entries(github)) {
    if (values[name] !== undefined) {
      identity[field] = positive(name, values[name]);
    }
  }
  if (values["attempt-id"] !== undefined) {
    identity.attemptId = values["attempt-id"];
  }
  const problems = attemptProblems(identity, Object.values(github));
  if (problems.length > 0) {
    throw new Error(`${command} ${problems.join("; ")}`);
  }
  return identity;
}

// A declared unit option's value: `case` or `file`.
export function declaredUnit(name, text) {
  if (!DECLARED_UNITS.includes(text)) {
    throw new Error(
      `--${name} names unit ${text}, not ${DECLARED_UNITS.join(" or ")}`,
    );
  }
  return text;
}

// `<entry>=<parts>` or `<entry>=<parts>:<unit>`.
export function parseExpect(declaration) {
  const equals = declaration.lastIndexOf("=");
  const entry = declaration.slice(0, equals);
  if (equals <= 0) {
    throw new Error(`--expect ${declaration} is not <entry>=<parts>[:<unit>]`);
  }
  const [parts, ...unit] = declaration.slice(equals + 1).split(":");
  return {
    entry,
    parts: positive("expect", parts),
    ...(unit.length === 0
      ? {}
      : { unit: declaredUnit(`expect ${declaration}`, unit.join(":")) }),
  };
}
