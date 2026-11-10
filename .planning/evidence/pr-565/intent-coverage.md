# Dough Intent Coverage

**Story:** SEED-054#process-pr-565
**Revision:** b676d6f224491aa45578f8493e6aee38c340e6ff
**Promise:** A class with a second annotation that has arguments (e.g. `@Entity @Table(name = "vets") class Vet`) still reports its methods rather than losing them.
**Fault:** Removed the `self._state(token)` pass-through in `JavaStates._state_post_decorator` after an unqualified annotation, restoring the ceb899aa token-drop behavior.
**Fixture/assertion:** `test_jpa_entity_with_table_annotation` and `test_class_with_annotation_arguments_after_another_annotation` in `test/test_languages/testJava.py`.
**Outcome:** Caught. Baseline green (2 passed); with the fault both tests failed on empty function lists; restoration returned green and a clean scratch tree.

## Costs

- **Test runtime.** Measured: focused pytest about 0.07s baseline, 0.03s fault, 0.03s restore.
- **Human effort.** Unavailable: no separate human effort measure in this session.
- **Agent effort.** Unavailable: no separate agent-effort meter in this session.


- **Invocation-to-report elapsed.** 22589 ms (measured), from 1794317295221 to 1794317317810 in Unix epoch ms. The end clock was sampled after writing the prepared report body.
