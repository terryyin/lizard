"""Check the changed assertion predicate at an export-default statement."""

import importlib.util
from pathlib import Path


spec = importlib.util.spec_from_file_location(
    'keyword_probe', Path(__file__).with_name('keyword_owner_probe.py'))
probe = importlib.util.module_from_spec(spec)
spec.loader.exec_module(probe)
probe.CASES = {
    word: 'export default ' + word + '((arg) => arg || 0);\n'
    for word in ('as', 'satisfies')}
probe.main()
