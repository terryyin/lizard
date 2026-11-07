// The rules of asking GitHub that every reader of an answer shares, whichever
// process or page reads it. No filesystem, Git, or Node-only imports, so a
// browser bundle can reference the exact same rules.

// The longest wait, in whole seconds, ever passed on from a GitHub rate
// limit's direction (`./github-request-answer.mjs`): GitHub's own primary
// rate-limit window. A header naming a later time is either wrong or will be
// repeated by the next refused answer, and a reader left waiting must still
// check again.
export const longestDirectedWaitSeconds = 60 * 60;
