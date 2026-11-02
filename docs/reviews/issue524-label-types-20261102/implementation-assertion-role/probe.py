import argparse,hashlib,importlib,json,sys
from pathlib import Path
parser=argparse.ArgumentParser();parser.add_argument('--checkout',type=Path,required=True);parser.add_argument('--out',type=Path,required=True);args=parser.parse_args()
root=args.checkout.resolve();sys.path.insert(0,str(root));lizard=importlib.import_module('lizard')
fixtures=json.loads((Path(__file__).parent/'fixtures.json').read_text());observations=[]
for fixture in fixtures:
 for suffix in fixture['suffixes']:
  source='<script lang="ts">'+fixture['source']+'</script>' if suffix=='vue' else fixture['source']
  reader=importlib.import_module('lizard_languages').get_reader_for('a.'+suffix)
  info=lizard.analyze_file.analyze_source_code('a.'+suffix,source)
  result=[[f.name,f.cyclomatic_complexity,f.start_line,f.end_line,f.parameters] for f in info.function_list]
  observations.append({**fixture,'suffix':suffix,'analyzed_source':source,'tokens':list(reader.generate_tokens(source)),'actual':result,'matched':result==fixture['expected']})
modules={}
for name,module in list(sys.modules.items()):
 if name=='lizard' or name.startswith(('lizard_core','lizard_languages','lizard_ext')):
  filename=getattr(module,'__file__',None)
  if filename:
   path=Path(filename).resolve();assert root in path.parents,(name,path);modules[name]={'path':str(path),'sha256':hashlib.sha256(path.read_bytes()).hexdigest()}
record={'command':'PIP_QUIET=3 nix develop -c python -I '+' '.join(sys.argv),'python':sys.version,'executable':sys.executable,'checkout':str(root),'modules':modules,'observations':observations,'matched':sum(x['matched'] for x in observations),'total':len(observations),'exit_code':0}
args.out.write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps({'matched':record['matched'],'total':record['total'],'mismatches':[{'name':x['name'],'suffix':x['suffix'],'actual':x['actual']} for x in observations if not x['matched']]}))
