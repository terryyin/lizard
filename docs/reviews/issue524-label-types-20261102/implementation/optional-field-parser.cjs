const fs = require('fs');
const crypto = require('crypto');
const tsPath = require.resolve('/tmp/issue524-typescript-parse/node_modules/typescript');
const ts = require(tsPath);
const sources = JSON.parse(fs.readFileSync(__dirname + '/optional-field-sources.json', 'utf8'));
const results = [];
for (const {field, source} of sources) {
  for (const kind of ['TS', 'TSX']) {
    const file = ts.createSourceFile('case.' + kind.toLowerCase(), source, ts.ScriptTarget.Latest, true, ts.ScriptKind[kind]);
    const member = file.statements[0].members[0];
    const output = ts.transpileModule(source, {fileName:file.fileName, reportDiagnostics:true, compilerOptions:{target:ts.ScriptTarget.ES2020}});
    results.push({field, kind, source, parseDiagnostics:file.parseDiagnostics.map(d=>({code:d.code,text:ts.flattenDiagnosticMessageText(d.messageText,'\n')})), memberName:member.name.getText(file), optional:!!member.questionToken, hasInitializer:!!member.initializer, transpileDiagnostics:output.diagnostics.map(d=>({code:d.code,text:ts.flattenDiagnosticMessageText(d.messageText,'\n')})), outputText:output.outputText});
  }
}
console.log(JSON.stringify({compiler:{version:ts.version,path:tsPath,sha256:crypto.createHash('sha256').update(fs.readFileSync(tsPath)).digest('hex')},results},null,2));
