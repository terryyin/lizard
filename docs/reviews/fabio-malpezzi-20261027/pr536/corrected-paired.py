"""Import guards around the byte-identical canonical paired algorithm.

Execute a real copy of this file from /private/tmp; never put a source archive
on sys.path before the canonical harness itself has loaded.
"""
import hashlib
import importlib.util
import json
from pathlib import Path
import sys

PROOF = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536-corrected/proof')
EXPECTED = 'ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520'
assert hashlib.sha256((PROOF / 'paired.py').read_bytes()).hexdigest() == EXPECTED
spec = importlib.util.spec_from_file_location('canonical_paired', PROOF / 'paired.py')
harness = importlib.util.module_from_spec(spec)
spec.loader.exec_module(harness)
original_worker = harness.worker


def guard(checkout, phase):
    root = Path(checkout).resolve()
    names = ['lizard', 'lizard_languages.javascript_bindings',
             'lizard_languages.javascript_functions',
             'lizard_languages.typescript_states', 'lizard_ext.lizardns']
    if (root / 'lizard_core').is_dir():
        names.extend(['lizard_core.function_info', 'lizard_core.file_analyzer'])
    paths = {}
    for name in names:
        module = __import__(name, fromlist=['__file__'])
        path = Path(module.__file__).resolve()
        assert root in path.parents, (name, str(path), str(root))
        paths[name] = str(path)
    print(json.dumps({'guard': phase, 'root': str(root), 'paths': paths}),
          file=sys.stderr, flush=True)


def checked_worker(checkout, loops, workload):
    sys.path.insert(0, str(Path(checkout).resolve()))
    guard(checkout, 'before')
    try:
        original_worker(checkout, loops, workload)
    finally:
        guard(checkout, 'after')


# The canonical subprocess launcher executes this same guarded script.
harness.__file__ = __file__
if len(sys.argv) > 1 and sys.argv[1] == 'worker':
    checked_worker(*sys.argv[2:])
else:
    harness.run()
