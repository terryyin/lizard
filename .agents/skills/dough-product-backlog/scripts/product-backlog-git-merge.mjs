#!/usr/bin/env node
import { runGitOperationCli } from "./product-backlog-git-cli.mjs";
import {
  continueMerge,
  mergeBacklog,
} from "./product-backlog-git-merge-operation.mjs";

await runGitOperationCli("merge", {
  start: mergeBacklog,
  resume: continueMerge,
});
