"""Public analyze_file observations for target vs corrected PR545 snapshots."""
import json
from pathlib import Path
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr545')
EVIDENCE = Path(__file__).resolve().parent
CASES = json.loads((EVIDENCE / 'inputs.json').read_text())


def observe(label):
    checkout = ROOT / label
    sys.path.insert(0, str(checkout))
    import lizard
    import lizard_languages.javascript_templates as templates
    import lizard_languages.typescript as typescript
    from lizard import analyze_file
    paths = {
        'lizard': lizard.__file__,
        'templates': templates.__file__,
        'typescript': typescript.__file__,
    }
    root = str(checkout.resolve()) + '/'
    for key, path in paths.items():
        assert str(Path(path).resolve()).startswith(root), (key, path)
    rows = []
    for case in CASES:
        functions = analyze_file.analyze_source_code(
            case['filename'], case['source']).function_list
        observed = [
            [f.name, f.cyclomatic_complexity, f.nloc] for f in functions]
        rows.append({
            'filename': case['filename'],
            'observed': observed,
            'expected': case['expected'],
            'matches': observed == case['expected'],
        })
    for key, path in paths.items():
        assert str(Path(path).resolve()).startswith(root), (key, path)
    sys.path.pop(0)
    for name in list(sys.modules):
        if name == 'lizard' or name.startswith('lizard.') or name.startswith(
                'lizard_') or name.startswith('lizard_languages') or name.startswith(
                'lizard_core') or name.startswith('lizard_ext'):
            del sys.modules[name]
    return {'label': label, 'paths': paths, 'rows': rows}


def main():
    report = {label: observe(label) for label in ('target', 'corrected')}
    (EVIDENCE / 'reproduction.json').write_text(json.dumps(report, indent=2) + '\n')
    summary = {
        label: {
            'all_match': all(row['matches'] for row in data['rows']),
            'mismatches': [
                row['filename'] for row in data['rows'] if not row['matches']],
            'mismatch_detail': [
                {'filename': row['filename'],
                 'observed': row['observed'],
                 'expected': row['expected']}
                for row in data['rows'] if not row['matches']],
        }
        for label, data in report.items()}
    (EVIDENCE / 'reproduction-summary.log').write_text(
        json.dumps(summary, indent=2) + '\n')
    print(json.dumps(summary, indent=2))


if __name__ == '__main__':
    main()
