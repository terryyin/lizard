# Source-only commitments

Fresh reviewer `/root/issue524_source_review` was launched with fork_turns:none.
Its complete source-only input and actual numbered return are retained in this
chat's collaboration launch/result. It was supplied only the human instruction
and canonical Goal/Scope/Known expectations/uncertainty, and told to read no
files, product, plan or proof, use no tools and change nothing. The following
80 returned commitment ids/text retain qualifications and journey examples.
Allocation and observations are maintained through the installed record operation.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| C01 | Function retention: Retain every runtime function affected by labeled blocks sharing enclosing brackets. | slice 1 |  |
| C02 | Metric retention: Retain those functions’ metrics. | slice 1 |  |
| C03 | Annotation function retention: Retain every runtime function affected by type annotations sharing enclosing brackets. | slice 1 |  |
| C04 | Annotation metric retention: Retain those functions’ metrics. | slice 1 |  |
| C05 | Ownership: Correct the distinction between runtime-label ownership and type-annotation ownership. | slice 1 |  |
| C06 | Termination: Correct annotation termination at enclosing runtime delimiters. | slice 1 |  |
| C07 | Replay: Correct replay of enclosing runtime delimiters to their runtime reader. | slice 1 |  |
| C08 | Field investigation: Investigate related function-type class-field cases. | slice 1 |  |
| C09 | Assertion investigation: Investigate related object-type assertion cases. | slice 1 |  |
| C10 | Related corrections: Correct confirmed failures within the same ownership concept. | slice 1 |  |
| C11 | Label interpretation: Treat a runtime labeled statement as runtime syntax. | slice 1 |  |
| C12 | Label continuation: Preserve later decisions within the labeled runtime block. | slice 1 |  |
| C13 | Statement continuation: Preserve later statements within that block. | slice 1 |  |
| C14 | Runtime closure: Close the block at its actual runtime brace. | slice 1 |  |
| C15 | Brace boundary: Prevent annotations from swallowing an unmatched enclosing-runtime `}`. | slice 1 |  |
| C16 | Bracket boundary: Prevent annotations from swallowing an unmatched enclosing-runtime `]`. | slice 1 |  |
| C17 | Comma boundary: Prevent annotations from swallowing an unmatched enclosing-runtime `,`. | slice 1 |  |
| C18 | Type brackets: Preserve legitimate nested type brackets. | slice 1 |  |
| C19 | Type commas: Preserve legitimate nested type commas. | slice 1 |  |
| C20 | Type arrows: Preserve legitimate function-type arrows. | slice 1 |  |
| C21 | Signatures: Preserve valid signatures. | slice 1 |  |
| C22 | Defaults: Preserve valid defaults. | slice 1 |  |
| C23 | Ternaries: Preserve valid ternaries. | slice 1 |  |
| C24 | Templates: Preserve valid templates. | slice 1 |  |
| C25 | Extension metrics: Preserve established extension metrics. | slice 1 |  |
| C26 | CLI output: Preserve established CLI output. | slice 1 |  |
| C27 | Earlier fixes: Preserve all previously fixed #524 examples. | slice 1 |  |
| C28 | Original examples: Preserve established behavior for the six original issue examples. | slice 1 |  |
| C29 | Authored PR examples: Preserve established behavior for all eleven authored PR #525 examples. | slice 1 |  |
| C30 | Suffixes: Preserve established behavior across the seven JS-family suffixes. | slice 1 |  |
| C31 | NS: Preserve established NS behavior. | slice 1 |  |
| C32 | Nonstrict: Preserve established nonstrict behavior. | slice 1 |  |
| C33 | CSV: Preserve established CSV-consumer behavior. | slice 1 |  |
| C34 | Output option: Preserve established `-o`-consumer behavior. | slice 1 |  |
| C35 | HTTP: Preserve established HTTP-consumer behavior. | slice 1 |  |
| C36 | Regression proof: Add discriminating regression tests. | slice 1 |  |
| C37 | Refactoring: Independently refactor the aggregate change. | slice 1 |  |
| C38 | API performance: Qualify affected API consumers. | slice 1 |  |
| C39 | Parameter performance: Qualify affected parameter consumers. | slice 1 |  |
| C40 | Extension performance: Qualify affected extension consumers. | slice 1 |  |
| C41 | CLI performance: Qualify broad CLI performance. | slice 1 |  |
| C42 | Pytest: Run the full pytest suite successfully. | slice 1 |  |
| C43 | Make: Run the full make checks successfully. | slice 1 |  |
| C44 | Hosted checks: Pass applicable hosted CI on the exact head. | slice 2 |  |
| C45 | Merge: Merge the exact qualified head into current master. | slice 2 |  |
| C46 | Closure: Close issue #524 after the required checks pass. | slice 2 |  |
| C47 | Summary: Provide the related-fix summary with issue closure. | slice 2 |  |
| C48 | Failure reporting: Report any remaining #524 failure. | slice 2 |  |
| C49 | Observation retention: Retain failed observations. | slice 1 |  |
| C50 | Timing retention: Retain immutable timing inputs. | slice 1 |  |
| C51 | Public meanings: Preserve public metric meanings. | slice 1 |  |
| C52 | Working tree: Preserve unrelated working-tree changes. | slice 1 |  |
| Q01 | Performance threshold: Each required performance qualification must satisfy a strict paired 95% upper slowdown bound below 3%; equality does not satisfy “below.” | slice 1 |  |
| Q02 | Related-case scope: Field/assertion corrections are conditional on confirming failures that exercise the same ownership responsibility. | slice 1 |  |
| Q03 | Ownership uncertainty: Determine the smallest existing reader/signature owners that distinguish runtime labels and own nested type syntax. | slice 1 |  |
| Q04 | Investigation order: Confirm the exact related field/assertion failures before choosing their implementation. | slice 1 |  |
| Q05 | Delivery ordering: Merge and issue closure follow discriminating regression proof, independent refactoring, all required performance gates, full local checks, and applicable exact-head hosted CI. | slice 2 |  |
| Q06 | Completeness: Partial correctness cannot be represented as grounds for closing #524. | slice 2 |  |
| Q07 | Authority: The human explicitly authorizes implementation. | slice 1 |  |
| Q08 | Publication authority: The human explicitly authorizes publication. | slice 1 |  |
| Q09 | PR authority: The human explicitly authorizes PR creation. | slice 2 |  |
| Q10 | Merge authority: The human explicitly authorizes merge. | slice 2 |  |
| Q11 | Issue authority: The human explicitly authorizes the issue summary and closure. | slice 2 |  |
| T01 | Minimal labeled-block example: `const x = [() => { a: 1 }];` must report one anonymous function. | slice 1 |  |
| T02 | Minimal-example location: That function must be reported at line 1. | slice 1 |  |
| T03 | Reference behavior: The minimal example must match the stated 1.24.1 behavior; the source reports zero functions on master `da29d5d`. | slice 1 |  |
| T04 | Successor name: A following function retains its own name. | slice 1 |  |
| T05 | Successor formals: A following function retains its own formals. | slice 1 |  |
| T06 | Successor span: A following function retains its own span. | slice 1 |  |
| T07 | Successor complexity: A following function retains its own CCN. | slice 1 |  |
| T08 | Field-to-constructor journey: A function-type class field preserves a following constructor when it exercises the shared ownership responsibility. | slice 1 |  |
| T09 | Field-to-method journey: A function-type class field preserves a following method under that qualification. | slice 1 |  |
| T10 | Assertion-to-arrow journey: An object-type assertion preserves expression-arrow boundaries under that qualification. | slice 1 |  |
| T11 | Assertion-to-successor journey: An object-type assertion preserves successor boundaries under that qualification. | slice 1 |  |
| T12 | Example reach: These examples demonstrate required behavior; they do not restrict the promise to only those spellings, delimiters, or successor forms. | slice 1 |  |
| X01 | Counter exclusion: Do not introduce a parallel runtime bracket counter. | slice 1 |  |
| X02 | Buffer exclusion: Do not introduce a whole-stream buffer. | slice 1 |  |
| X03 | Switch exclusion: Do not introduce an issue-specific switch. | slice 1 |  |
| X04 | Premature delivery exclusion: Do not merge before the stated gates pass. | slice 2 |  |
| X05 | Premature closure exclusion: Do not close #524 before the stated gates pass. | slice 2 |  |
