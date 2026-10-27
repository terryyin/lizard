"""Copy complete new evidence after terminal measurement without rerunning it."""

import json
import shutil

from root_qualification_common import (
    CACHE, GATES, HERE, assert_inputs, digest, now, write_json)


def main():
    assert_inputs()
    summary = json.loads((CACHE / 'summary.json').read_text())
    assert len(summary['gates']) == 6
    copied = {}
    names = ['freeze.json', 'decision.json', 'supplemental-inputs.json',
             'original-corpus-hashes.json', 'augmented-corpus-hashes.json',
             'candidate-source-hashes.json', 'current-source-hashes.json',
             'frozen-source-hashes.json', 'freeze-driver-receipt.json',
             'measure-driver-receipt.json', 'summary.json']
    names += [label + '.json' for label, _, _, _ in GATES]
    for name in names:
        target = HERE / ('root-' + name)
        assert not target.exists(), target
        if name.endswith('corpus-hashes.json'):
            value = json.loads((CACHE / name).read_text())
            affected = {key: item for key, item in value.items() if key.startswith('affected/')}
            broad = {key: item for key, item in value.items() if not key.startswith('affected/')}
            for suffix, part in (('affected', affected), ('broad', broad)):
                output = HERE / ('root-' + name.replace('corpus', suffix))
                assert not output.exists()
                write_json(output, part)
                copied[output.name] = digest(output)
        else:
            shutil.copyfile(CACHE / name, target)
            copied[target.name] = digest(target)
    receipts = {}
    for path in sorted(CACHE.glob('*-receipt.json')):
        value = json.loads(path.read_text())
        assert value['returncode'] in (0, 1), value
        receipts[path.name] = dict(returncode=value['returncode'], sha256=digest(path))
    payload = dict(finished=now(), terminal_receipts=receipts,
                   all_six_measurements_exit_zero=all(
                       receipts[label + '-terminal-receipt.json']['returncode'] == 0
                       for label, _, _, _ in GATES),
                   qualification=summary['all_pass'], files=copied,
                   postcheck_returncode=0)
    write_json(HERE / 'root-postcheck.json', payload)
    print(json.dumps(dict(all_six_measurements_exit_zero=True,
                          all_six_bounds_pass=summary['all_pass'],
                          postcheck_returncode=0)), flush=True)


if __name__ == '__main__':
    main()
