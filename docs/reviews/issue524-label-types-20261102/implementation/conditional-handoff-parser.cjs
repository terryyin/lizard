const fs = require('fs');
const crypto = require('crypto');
const tsPath = require.resolve('/tmp/issue524-typescript-parse/node_modules/typescript');
const ts = require(tsPath);
const sources = JSON.parse(fs.readFileSync(__dirname + '/conditional-handoff-sources.json', 'utf8'));
const results = [];
for (const [name, source] of Object.entries(sources)) {
  for (const kind of ['TS', 'TSX']) {
    const file = ts.createSourceFile('case.' + kind.toLowerCase(), source, ts.ScriptTarget.Latest, true, ts.ScriptKind[kind]);
    const initializer = file.statements[0].declarationList.declarations[0].initializer;
    const output = ts.transpileModule(source, {fileName: file.fileName, reportDiagnostics: true, compilerOptions: {target: ts.ScriptTarget.ES2020}});
    results.push({name, kind, source, parseDiagnostics: file.parseDiagnostics.map(d => ({code: d.code, text: ts.flattenDiagnosticMessageText(d.messageText, '\n')})), initializerKind: ts.SyntaxKind[initializer.kind], condition: initializer.condition.getText(file), trueBranch: initializer.whenTrue.getText(file), falseBranch: initializer.whenFalse.getText(file), transpileDiagnostics: output.diagnostics.map(d => ({code: d.code, text: ts.flattenDiagnosticMessageText(d.messageText, '\n')})), outputText: output.outputText});
  }
}
console.log(JSON.stringify({compiler: {version: ts.version, path: tsPath, sha256: crypto.createHash('sha256').update(fs.readFileSync(tsPath)).digest('hex'), documentation: 'https://www.typescriptlang.org/docs/handbook/2/conditional-types.html'}, results}, null, 2));
