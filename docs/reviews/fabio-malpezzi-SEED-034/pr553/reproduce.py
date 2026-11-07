from lizard import analyze_file
code = """
class A {
  'max-age'?: number;
  m() { return 1; }
}
function f(e: unknown, o: { "a-b"?: number }) {
  const h = (e as { 'retry-after'?: string })['retry-after'];
  return h || 'x';
}
function g(c) { return c?'a':'b'; }
"""
r = analyze_file.analyze_source_code('x.ts', code)
print([(f.name, f.cyclomatic_complexity) for f in r.function_list])
