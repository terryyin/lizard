#!/usr/bin/env node
import { runGitOperationCli } from "./product-backlog-git-cli.mjs";
import {
  continueRebase,
  rebaseBacklog,
  validateRebase,
} from "./product-backlog-git-rebase-operation.mjs";

await runGitOperationCli("rebase", {
  start: rebaseBacklog,
  resume: continueRebase,
  validate: validateRebase,
});
