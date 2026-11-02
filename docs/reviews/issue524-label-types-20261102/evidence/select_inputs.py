"""Freeze actual fixture bytes, retaining each consuming-context provenance."""

import argparse
import hashlib
import json
from pathlib import Path


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--capture', type=Path, required=True)
    parser.add_argument('--out', type=Path, required=True)
    args = parser.parse_args()
    cases = json.loads(args.capture.read_text())
    selected = {}
    for index, case in enumerate(cases):
        source = case['analyzed_source']
        suffix = case['suffix']
        assert source and suffix in ('js', 'ts', 'jsx', 'tsx', 'mjs', 'cjs', 'vue')
        if suffix == 'vue':
            assert '<script' in source
        key = (case['family'], suffix, source)
        if key not in selected:
            family = case['family']
            digest = hashlib.sha256(source.encode()).hexdigest()[:16]
            selected[key] = {'family': family,
                             'filename': digest + '.' + suffix,
                             'source': source, 'provenance': []}
        selected[key]['provenance'].append({
            'capture_index': index, 'test': case['test'],
            'entry': case['entry'], 'extension': case.get('extension'),
            'original_filename': case.get('filename')})
    result = list(selected.values())
    with args.out.open('x') as output:
        json.dump(result, output, indent=2)
        output.write('\n')
    print(json.dumps({'captured': len(cases), 'frozen_sources': len(result),
                      'families': {f: sum(r['family'] == f for r in result)
                                   for f in ('labels', 'annotations')}}))


if __name__ == '__main__':
    main()
