# Consumer-trace dispositions — SEED-035 slice 2

Source: `consumer-trace.txt` against base `43a07aafa2e1` with terms
`_member_name`, `NestedStructure`, `max_nested_structures`.

Product change is confined to `lizard_ext/lizardns.py` (member `.`/`?.`
skip) and `test/testJavaScriptNestedStructures.py` (discriminating test).

## Sufficient proof selected

| Consumer | Disposition |
| --- | --- |
| `test/testJavaScriptNestedStructures.py` | **run**: discriminating red/green + focused file (4 passed) |
| `test/testNestedStructures.py` | **run**: covered by full `pytest` (3038 passed) |
| `test/testNestedStructureScopes.py` | **run**: covered by full `pytest` |
| `test/testPythonNestedStructures.py` | **run**: covered by full `pytest` |
| `test/testExtension.py` | **run**: covered by full `pytest` |
| `test/test_languages/testJavaScriptBindingBoundaries.py` | **run**: covered by full `pytest` |
| `test/test_languages/testTypeScriptAnnotationBoundaries.py` | **run**: covered by full `pytest` |
| `lizard_core/option_resolution.py` | **excluded**: threshold alias wiring for `NS` → `max_nested_structures`; not reached by `_member_name` token state |
| Review manifests / prior attempt JSON / guidance readers | **excluded**: documentary or hash references; no product boundary |

Paired `--extension ns` workloads below prove public NS analysis performance
for the same extension surface.
