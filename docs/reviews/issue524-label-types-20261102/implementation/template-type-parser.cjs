const fs=require('fs');
const tsPath=require.resolve('/tmp/issue524-typescript-parse/node_modules/typescript');
const ts=require(tsPath);
const crypto=require('crypto');
const sources=JSON.parse(fs.readFileSync(__dirname+'/template-type-sources.json','utf8'));
const results=[];
for(const [name,source] of Object.entries(sources)) for(const kind of ['TS','TSX']) {
 const file=ts.createSourceFile('case.'+kind.toLowerCase(),source,ts.ScriptTarget.Latest,true,ts.ScriptKind[kind]);
 const output=ts.transpileModule(source,{fileName:file.fileName,reportDiagnostics:true,compilerOptions:{target:ts.ScriptTarget.ES2020}});
 results.push({name,kind,source,parseDiagnostics:file.parseDiagnostics.map(d=>({code:d.code,text:ts.flattenDiagnosticMessageText(d.messageText,'\n')})),transpileDiagnostics:output.diagnostics.map(d=>({code:d.code,text:ts.flattenDiagnosticMessageText(d.messageText,'\n')})),outputText:output.outputText});
}
console.log(JSON.stringify({compiler:{version:ts.version,path:tsPath,sha256:crypto.createHash('sha256').update(fs.readFileSync(tsPath)).digest('hex')},results},null,2));
