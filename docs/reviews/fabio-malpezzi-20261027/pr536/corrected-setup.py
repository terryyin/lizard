"""Freeze source archives and every corrected public regression before timing."""
import ast
import hashlib
import io
import json
from pathlib import Path
import shutil
import subprocess
import tarfile

CHECKOUT = Path.cwd()
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536')
ROOT = CACHE.with_name('pr536-corrected')
REVISIONS = {
    'baseline': '2e21e404c1ab9f4e0b49acc80816e816629d0670',
    'target': '9979f106a955aef2460ed6fe7ebb5782b755bcad',
    'candidate-0': '9276aea2aa337c070169ba40f88b8ce207530243',
}


def sha(data):
    return hashlib.sha256(data).hexdigest()


def manifest(path):
    return {str(p.relative_to(path)): sha(p.read_bytes())
            for p in sorted(path.rglob('*')) if p.is_file()
            and '__pycache__' not in p.parts and p.suffix != '.pyc'}


def product(path):
    return {name: digest for name, digest in manifest(path).items()
            if name == 'lizard.py' or name.startswith(
                ('lizard_core/', 'lizard_languages/', 'lizard_ext/'))}


def freeze():
    ROOT.mkdir(exist_ok=False)
    for name, revision in REVISIONS.items():
        dest = ROOT / name
        dest.mkdir()
        archive = subprocess.check_output(['git', 'archive', revision])
        with tarfile.open(fileobj=io.BytesIO(archive)) as stream:
            stream.extractall(dest)
        (ROOT / (name + '-product.json')).write_text(
            json.dumps(product(dest), sort_keys=True) + '\n')
    proof = ROOT / 'proof'
    proof.mkdir()
    source = CHECKOUT / 'docs/reviews/fabio-malpezzi-20261027/paired.py'
    intended = 'ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520'
    assert sha(source.read_bytes()) == intended
    shutil.copyfile(source, proof / 'paired.py')
    shutil.copytree(CACHE / 'proof/corpus', proof / 'corpus')
    inputs = json.loads((CACHE / 'all-inputs.json').read_text())
    affected = proof / 'corpus/affected'
    assert len(inputs) == 65
    assert len(list(affected.iterdir())) == 65
    for case in inputs:
        assert (affected / case['filename']).read_bytes() == case['source'].encode()
    # The original broad corpus remains exactly the 100 frozen Python sources.
    broad = {k: v for k, v in manifest(proof / 'corpus').items()
             if not k.startswith('affected/')}
    assert len(broad) == 100
    old_broad = json.loads((CACHE / 'proof/manifest.json').read_text())
    assert all(old_broad[name] == value for name, value in broad.items())
    supplements = []
    for filename in ['testJavaScriptArrowParameters.py',
                     'testJavaScriptFormalSlots.py']:
        tree = ast.parse((CHECKOUT / 'test/test_languages' / filename).read_text())
        for test in tree.body:
            if not isinstance(test, ast.FunctionDef) or not test.name.startswith('test_'):
                continue
            extensions = ['js']
            for decorator in test.decorator_list:
                if (isinstance(decorator, ast.Call) and len(decorator.args) > 1
                        and isinstance(decorator.args[0], ast.Constant)
                        and decorator.args[0].value == 'extension'):
                    extensions = [x.lstrip('.') for x in ast.literal_eval(decorator.args[1])]
            constants = {}
            for node in ast.walk(test):
                if isinstance(node, ast.Assign) and isinstance(node.value, ast.Constant):
                    for target in node.targets:
                        if isinstance(target, ast.Name):
                            constants[target.id] = node.value.value
            for call in ast.walk(test):
                if not isinstance(call, ast.Call):
                    continue
                args = None
                if isinstance(call.func, ast.Name) and call.func.id == 'functions':
                    args = call.args[:1]
                elif (isinstance(call.func, ast.Attribute)
                      and call.func.attr == 'analyze_source_code'):
                    args = call.args[1:2]
                if not args:
                    continue
                value = args[0]
                source = value.value if isinstance(value, ast.Constant) else (
                    constants.get(value.id) if isinstance(value, ast.Name) else None)
                if not isinstance(source, str):
                    continue
                # Preserve each public regression's actual language setup.
                for extension in extensions:
                    text = '<script>\n' + source + '\n</script>' if extension == 'vue' else source
                    name = 'supp-' + test.name[5:] + '.' + extension
                    target = affected / name
                    if target.exists():
                        assert target.read_text() == text
                        continue
                    target.write_text(text)
                    supplements.append({'filename': name, 'test': filename + '::' + test.name})
    # Parameterized expression fixtures are frozen as complete source, too.
    patterns = ['{key: renamed, other}, [head, ...tail], last',
                '{outer: {key: renamed}}, tail', "{'key': renamed}, tail",
                '{[key]: renamed}, tail', '$, $$, a$, a$b, _, _x, $x, x1, café']
    for i, pattern in enumerate(patterns):
        name = 'supp-pattern-' + str(i) + '.js'
        (affected / name).write_text('run((' + pattern + ') => { return 1; });')
        supplements.append({'filename': name, 'test': 'test_anonymous_patterns_preserve_named_convention'})
    (proof / 'manifest.json').write_text(json.dumps(manifest(proof / 'corpus'), sort_keys=True) + '\n')
    (ROOT / 'supplements.json').write_text(json.dumps(supplements) + '\n')
    (ROOT / 'identities.json').write_text(json.dumps({
        'revisions': REVISIONS, 'harness_sha256': intended,
        'old_cached_harness_sha256': sha((CACHE / 'proof/paired.py').read_bytes()),
        'inputs_sha256': sha((CACHE / 'all-inputs.json').read_bytes()),
        'manifest_sha256': sha((proof / 'manifest.json').read_bytes()),
        'original_affected': len(inputs), 'supplements': len(supplements),
        'broad': len(broad), 'affected': len(list(affected.iterdir())),
    }, sort_keys=True) + '\n')
    print((ROOT / 'identities.json').read_text())


if __name__ == '__main__':
    freeze()
