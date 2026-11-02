const fs=require('fs');const crypto=require('crypto');const path=require.resolve('/tmp/issue524-typescript-parse/node_modules/typescript');const ts=require(path);
const fixtures=JSON.parse(fs.readFileSync(__dirname+'/fixtures.json','utf8'));const rows=[];
for(const f of fixtures) for(const kind of ['TS','TSX']){const file=ts.createSourceFile('a.'+kind.toLowerCase(),f.source,ts.ScriptTarget.Latest,true,ts.ScriptKind[kind]);rows.push({name:f.name,kind,source:f.source,diagnostics:file.parseDiagnostics.map(d=>({code:d.code,text:ts.flattenDiagnosticMessageText(d.messageText,'\n')}))});}
console.log(JSON.stringify({version:ts.version,path,sha256:crypto.createHash('sha256').update(fs.readFileSync(path)).digest('hex'),observations:rows},null,2));
