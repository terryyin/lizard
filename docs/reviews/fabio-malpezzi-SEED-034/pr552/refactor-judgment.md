# Independent post-change refactor judgment — PR552

**Decision:** none.

**Reason:** The defect is a single resume-state mismatch after consuming
declaration generics. The fix adds an optional `resume` argument to the existing
`_consume_generic_type_params` owner and passes `self._function` from the one
call site that must stay in `_function`. Other callers (`javascript_objects`,
`typescript_states`) correctly keep the default `_state_global` resume.
No second representation of generic-parameter consumption was introduced, and
no untouched parallel helper needs consolidation for this change.
