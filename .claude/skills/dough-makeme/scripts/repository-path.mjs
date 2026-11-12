// Safe repository-relative paths, shared by configured selection and admission.
/** @param {string} path */
export function isSafeRepositoryPath(path) {
  if (path.length === 0 || path.startsWith("/") || path.includes("\\"))
    return false;
  return !path
    .split("/")
    .some((segment) => segment === "" || segment === "." || segment === "..");
}
