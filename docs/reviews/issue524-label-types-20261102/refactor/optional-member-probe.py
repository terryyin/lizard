from pathlib import Path
import hashlib
import json
from test.test_languages.testJavaScriptBindingBoundaries import functions, summaries

root = Path('docs/reviews/issue524-label-types-20261102/refactor')
after = 'function after(tail) { return tail ? 1 : 0; }\n'
fixtures = {
    'optional-type-only': 'class A { d?: (()=>void)[]; constructor(v){if(v)use(v);} s(tail){return tail?1:0;} }\n' + after,
    'optional-typed-runtime-ternary': 'class A { d?: (n:number)=>number = flag ? function yes(a){return a?1:0;} : function no(b){return b?1:0;}; constructor(v){if(v)use(v);} s(tail){return tail?1:0;} }\n' + after,
    'required-typed-runtime-ternary': 'class A { d: (n:number)=>number = flag ? function yes(a){return a?1:0;} : function no(b){return b?1:0;}; constructor(v){if(v)use(v);} s(tail){return tail?1:0;} }\n' + after,
}
methods = [('constructor', 2, 1, 1, ['v']), ('s', 2, 1, 1, ['tail']), ('after', 2, 2, 2, ['tail'])]
runtime = [('yes', 2, 1, 1, ['a']), ('no', 2, 1, 1, ['b'])]
observations = []
for name, source in fixtures.items():
    expected = methods if name == 'optional-type-only' else runtime + methods
    for suffix in ('ts', 'tsx', 'vue'):
        actual = summaries(functions(source, suffix))
        observations.append({'case': name, 'suffix': suffix, 'source': source, 'expected': expected, 'actual': actual, 'matched': actual == expected})
record = {'entry_point': 'analyze_file.analyze_source_code through functions(source,suffix), real Vue <script lang="ts"> wrapper', 'command': 'PIP_QUIET=3 nix develop -c python - <<\'PY\' (the literal probe source is retained in optional-member-probe.py) PY', 'signature_sha256': hashlib.sha256(Path('lizard_languages/typescript_signatures.py').read_bytes()).hexdigest(), 'observations': observations, 'passed': sum(item['matched'] for item in observations), 'failed': sum(not item['matched'] for item in observations)}
(root / 'optional-member-probe.json').write_text(json.dumps(record, indent=2) + '\n')
print(json.dumps({'passed': record['passed'], 'failed': record['failed'], 'observations': [{k:v for k,v in item.items() if k != 'source'} for item in observations]}, indent=2))
assert all(item['matched'] for item in observations)
