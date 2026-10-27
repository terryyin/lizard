"""Freeze the committed dispatch revision with both unchanged affected corpora."""

import io
import json
import shutil
import subprocess
import sys
import tarfile

from dispatch_qualification_common import (
    CACHE, CHECKOUT, DRIVERS, GATES, HARNESS, HERE, INITIAL, MANIFEST,
    REVISIONS, assert_inputs, command, corpus_hashes, digest,
    now, observations, source_hashes, write_json)


def main():
    assert command('git', 'rev-parse', 'HEAD') == REVISIONS['candidate']
    assert command('git', 'rev-parse', 'origin/master') == REVISIONS['current']
    assert not command('git', 'diff', '--name-only', '--', 'lizard.py',
                       'lizard_core', 'lizard_languages', 'lizard_ext', 'test')
    assert not CACHE.exists(), 'A freeze is never overwritten'
    old = json.loads((INITIAL / 'freeze.json').read_text())
    assert old['revisions']['current'] == REVISIONS['current']
    assert old['revisions']['frozen'] == REVISIONS['frozen']
    assert digest(INITIAL / 'manifest.json') == MANIFEST
    inputs = {}
    for name, count in (('original', 128), ('supplemental', 38), ('augmented', 166)):
        path = INITIAL / (name + '-inputs.json')
        inputs[name] = json.loads(path.read_text())
        assert len(inputs[name]) == count
        old_key = {'original': 'original_inputs_sha256',
                   'supplemental': 'supplement_sha256',
                   'augmented': 'augmented_inputs_sha256'}[name]
        assert digest(path) == old[old_key]
    assert inputs['original'] + inputs['supplemental'] == inputs['augmented']
    for name, count in (('original', 228), ('augmented', 266)):
        runner = INITIAL / name
        assert digest(runner / 'paired.py') == HARNESS
        assert digest(runner / 'manifest.json') == MANIFEST
        hashes = json.loads((INITIAL / (name + '-corpus-hashes.json')).read_text())
        assert len(hashes) == count
        assert corpus_hashes(runner / 'corpus') == hashes
    CACHE.mkdir()
    for name in ('original', 'supplemental', 'augmented'):
        shutil.copyfile(INITIAL / (name + '-inputs.json'), CACHE / (name + '-inputs.json'))
    for name in ('manifest.json', 'cli-reference.txt'):
        shutil.copyfile(INITIAL / name, CACHE / name)
    for name in ('original', 'augmented'):
        shutil.copytree(INITIAL / name, CACHE / name)
        shutil.copyfile(INITIAL / (name + '-corpus-hashes.json'),
                        CACHE / (name + '-corpus-hashes.json'))
    for label, revision in REVISIONS.items():
        snapshot = CACHE / label
        snapshot.mkdir()
        archive = subprocess.check_output(['git', 'archive', revision], cwd=CHECKOUT)
        with tarfile.open(fileobj=io.BytesIO(archive)) as tree:
            tree.extractall(snapshot)
        write_json(CACHE / (label + '-source-hashes.json'), source_hashes(snapshot))
    shutil.copyfile(HERE / 'qualification-verify.py', CACHE / 'qualification-verify.py')
    host = INITIAL.parent / 'seed015-dispatch-13c633b8-host-before.json'
    shutil.copyfile(host, CACHE / 'host-before.json')
    freeze = dict(time=now(), revisions=REVISIONS, gates=GATES,
                  harness_sha256=HARNESS, original_manifest_sha256=MANIFEST,
                  drivers={name: digest(HERE / name) for name in DRIVERS},
                  original_inputs=128, supplement_inputs=38, augmented_inputs=166,
                  broad_files=100, python=sys.version, executable=sys.executable,
                  original_inputs_sha256=digest(CACHE / 'original-inputs.json'),
                  supplement_sha256=digest(CACHE / 'supplemental-inputs.json'),
                  augmented_inputs_sha256=digest(CACHE / 'augmented-inputs.json'),
                  cli_reference_sha256=digest(CACHE / 'cli-reference.txt'),
                  prior_freeze_sha256=digest(INITIAL / 'freeze.json'),
                  cli_expected_change='Exact corpus prefix substitution; all three new raw outputs equal')
    write_json(CACHE / 'freeze.json', freeze)
    assert_inputs()
    receipt = observations('before')
    assert_inputs()
    write_json(CACHE / 'freeze-observations.json', receipt)
    write_json(CACHE / 'freeze-driver-receipt.json', dict(finished=now(), returncode=0))
    print(json.dumps(dict(frozen=REVISIONS, original=128, supplemental=38,
                          augmented=166, public_candidate_expected=True,
                          all_seven_fields_match=True, returncode=0)), flush=True)


if __name__ == '__main__':
    main()
