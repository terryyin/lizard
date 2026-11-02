import hashlib, json
from pathlib import Path
import lizard
import lizard_languages.typescript as ts
from lizard import analyze_file
source = 'const f = (x: T extends number ? {v: T} : string[], y = flag ? 1 : 0): T extends number ? {v: T} : string[] => x;\nfunction after(tail) { return tail ? 1 : 0; }\n'
modules = {m.__name__: {'path': m.__file__, 'sha256': hashlib.sha256(Path(m.__file__).read_bytes()).hexdigest()} for m in (lizard, ts)}
results={}
for suffix in ('ts','tsx','vue'):
 s='<script lang="ts">'+source+'</script>' if suffix=='vue' else source
 results[suffix]=[(f.name,f.cyclomatic_complexity,f.start_line,f.end_line,f.parameters) for f in analyze_file.analyze_source_code('a.'+suffix,s).function_list]
print(json.dumps({'modules':modules,'source':source,'results':results},indent=2))
