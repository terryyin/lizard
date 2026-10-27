"""Observe new trunk's real HTTP adapter with immutable corrected analyzer bytes."""
import importlib
import json
from pathlib import Path
import subprocess
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
if len(sys.argv) > 1:
    checkout = Path(sys.argv[1]).resolve()
    sys.path.insert(0, str(checkout))
    from index import app
    paths = {}
    for name in ('index', 'lizard', 'lizard_languages',
                 'lizard_languages.javascript_functions',
                 'lizard_languages.typescript_states', 'lizard_core.function_info'):
        module = importlib.import_module(name)
        assert Path(module.__file__).resolve().is_relative_to(checkout), module.__file__
        paths[name] = module.__file__
    client = app.test_client()
    cases = [('.js', 'function render(text, _key, $el) {}', 'render', 3),
             ('.ts', 'function round(number: number, precision: number) {}', 'round', 2),
             ('.js', 'function names($, $$, a$) {}', 'names', 3),
             ('.js', "function unpack({'key': renamed}, tail) {}", 'unpack', 2)]
    rows = []
    for extension, code, name, count in cases:
        response = client.post('/analyze', json=dict(lang=extension, code=code))
        assert response.status_code == 200, response.data
        function, = response.get_json()['function_list']
        assert (function['name'], function['parameter_count']) == (name, count), function
        rows.append(dict(language=extension, status=response.status_code,
                         name=name, parameters=function['parameter_count']))
    print(json.dumps(dict(python=sys.version, executable=sys.executable,
                          imports=paths, observations=rows)))
else:
    import hashlib
    import io
    import os
    import shutil
    import tarfile
    repo = Path(__file__).resolve().parents[4]
    checkout = ROOT / 'integrated-website'
    if not checkout.exists():
        checkout.mkdir()
        data = subprocess.check_output(['git', 'archive',
            'd8e8e9af75b6d05da005e981af5f8867cced5d55'], cwd=repo)
        tarfile.open(fileobj=io.BytesIO(data)).extractall(checkout)
    hashes = {}
    for relative in ('lizard_core/function_info.py',
                     'lizard_languages/javascript_bindings.py',
                     'lizard_languages/javascript_functions.py',
                     'lizard_languages/typescript.py', 'lizard_languages/tsx.py'):
        shutil.copyfile(ROOT / 'corrected' / relative, checkout / relative)
        assert (repo / relative).read_bytes() == (checkout / relative).read_bytes()
        hashes[relative] = hashlib.sha256((checkout / relative).read_bytes()).hexdigest()
    interpreter = '/private/tmp/lizard-website-revival/.venv/bin/python'
    try:
        result = subprocess.run([interpreter, str(Path(__file__).resolve()), str(checkout)],
                                cwd='/private/tmp', text=True,
                                env=dict(os.environ, PYTHONDONTWRITEBYTECODE='1'),
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    except FileNotFoundError as error:
        payload = dict(observed=False, interpreter=interpreter, error=str(error),
                       product_hashes=hashes,
                       gap='Website runtime was retired; Flask is absent from selected/default checkout environments. HTTP observation remains pending integration.')
        (ROOT / 'website-consumer.json').write_text(json.dumps(payload, indent=2) + '\n')
        print(json.dumps(payload))
        sys.exit(0)
    payload = dict(command=[interpreter, str(Path(__file__).resolve()), str(checkout)],
                   product_hashes=hashes, exit=result.returncode, output=result.stdout)
    (ROOT / 'website-consumer.json').write_text(json.dumps(payload, indent=2) + '\n')
    print(json.dumps(payload))
    assert result.returncode == 0, result.stdout
