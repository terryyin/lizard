"""Capture actual regression fixtures without replacing any analyzer logic."""
import contextlib
import hashlib
import io
import json
import sys
from pathlib import Path

import pytest

out = Path(__file__).resolve().parent
cases = []
active_helpers = {}


def observe(frame, event, _arg):
    filename, test = frame.f_code.co_filename, frame.f_code.co_name
    if (event == 'call' and filename.endswith('testJavaScriptBindingBoundaries.py')
            and test == 'functions'):
        data = frame.f_locals
        test = frame.f_back.f_code.co_name
        cases.append({'test': test,
                      'family': 'labels' if 'label' in test else 'annotations',
                      'source': data['source'], 'suffix': data['suffix'],
                      'extension': data.get('extension'),
                      'entry': 'analyze_source_code'})
        active_helpers[id(frame)] = cases[-1]
    elif (event == 'return' and filename.endswith('testJavaScriptBindingBoundaries.py')
          and test == 'functions'):
        data = frame.f_locals
        case = active_helpers.pop(id(frame))
        case.update(analyzed_source=data['source'], filename='bindings.' + data['suffix'],
                    helper_path=filename, observed=[{
                        'name': f.name, 'ccn': f.cyclomatic_complexity,
                        'parameters': f.parameters, 'start_line': f.start_line,
                        'end_line': f.end_line, 'nloc': f.nloc, 'tokens': f.token_count
                    } for f in _arg])
    elif (event == 'return' and filename.endswith('testTypeScriptAnnotationBoundaries.py')
          and test.startswith('test_')):
        data = frame.f_locals
        if 'source' in data and any(kind in test for kind in ('cli', 'http', 'vue_script_mode')):
            cases.append({'test': test,
                          'family': 'labels' if 'label' in test else 'annotations',
                          'source': data['source'], 'analyzed_source': data['source'],
                          'suffix': data.get('suffix', 'vue'),
                          'entry': 'CLI CSV -o' if 'cli' in test else
                                   'HTTP' if 'http' in test else 'analyze_source_code'})


log = io.StringIO()
sys.setprofile(observe)
with contextlib.redirect_stdout(log), contextlib.redirect_stderr(log):
    status = pytest.main(['test/test_languages/testTypeScriptAnnotationBoundaries.py',
                          'test/test_languages/testTypeScriptAssertionRoles.py', '-q'])
sys.setprofile(None)
(out / 'cases-after-export.json').write_text(json.dumps(cases, indent=2) + '\n')
(out / 'fixture-capture-after-export.json').write_text(json.dumps({
    'command': 'PIP_QUIET=3 nix develop -c python ' + str(Path(__file__).resolve()),
    'exit_code': int(status), 'case_count': len(cases), 'stdout': log.getvalue(),
    'purpose': 'Observe actual parameterized public fixture setup and return; no parser logic replaced.',
    'modules': {name: {'path': sys.modules[name].__file__,
                       'sha256': hashlib.sha256(Path(sys.modules[name].__file__).read_bytes()).hexdigest()}
                for name in ('lizard', 'lizard_languages.typescript')}
}, indent=2) + '\n')
print('captured', len(cases), 'terminal exit', int(status))
