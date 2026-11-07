export const isFullGitRevision = (value) =>
  typeof value === "string" && /^[0-9a-f]{40}$/i.test(value);
