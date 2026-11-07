import { useRef, useState } from "react";
import Arrow from "./Arrow.jsx";

const TIMEOUT_MS = 20000;
const DEFAULT_LANGUAGE = ".py";
const EXAMPLES = {
  ".py": "def shipping_cost(order):\n    if order.is_pickup:\n        return 0\n    if order.total >= 50:\n        return 0\n    return 5",
  ".c": "int shipping_cost(int pickup, int total) {\n    if (pickup) return 0;\n    if (total >= 50) return 0;\n    return 5;\n}",
  ".js": "function shippingCost(order) {\n    if (order.isPickup) return 0;\n    if (order.total >= 50) return 0;\n    return 5;\n}",
  ".ts": "function shippingCost(pickup: boolean, total: number): number {\n    if (pickup) return 0;\n    if (total >= 50) return 0;\n    return 5;\n}",
  ".java": "class Shipping {\n    int cost(boolean pickup, int total) {\n        if (pickup) return 0;\n        if (total >= 50) return 0;\n        return 5;\n    }\n}",
  ".go": "package shipping\n\nfunc Cost(pickup bool, total int) int {\n    if pickup { return 0 }\n    if total >= 50 { return 0 }\n    return 5\n}",
  ".rs": "fn shipping_cost(pickup: bool, total: i32) -> i32 {\n    if pickup { return 0; }\n    if total >= 50 { return 0; }\n    5\n}",
  ".rb": "def shipping_cost(pickup, total)\n  return 0 if pickup\n  return 0 if total >= 50\n  5\nend",
};
const METRICS = ["cyclomatic_complexity", "nloc", "token_count", "parameter_count"];

function errorMessage(error) {
  if (error.name === "AbortError") return "Analysis took too long. Try a smaller sample or run Lizard locally.";
  if (error instanceof TypeError || error instanceof SyntaxError) {
    return "Unable to reach the analyzer. Please try again or run Lizard locally.";
  }
  return error.message;
}

function resultMessage(result) {
  const count = result.function_list.length;
  return count
    ? `Analyzed ${result.filename}. ${count} function${count === 1 ? "" : "s"} found.`
    : "Analysis complete. No functions were found. Try a complete function or check the selected language.";
}

const BADGES = { idle: "Ready when you are", pending: "Working…", success: "Analysis complete", error: "Try again" };

export default function Playground({ config }) {
  const [language, setLanguage] = useState(DEFAULT_LANGUAGE);
  const [source, setSource] = useState(EXAMPLES[DEFAULT_LANGUAGE]);
  const [report, setReport] = useState({ phase: "idle", message: "Run the example to explore its function metrics." });
  const sourceField = useRef(null);
  const pending = report.phase === "pending";
  const hasExample = Object.hasOwn(EXAMPLES, language);
  const fail = (message) => setReport({ phase: "error", message });

  function loadExample() {
    setSource(EXAMPLES[language]);
    sourceField.current.focus();
  }

  async function analyze(event) {
    event.preventDefault();
    if (pending) return;
    if (!source.trim()) {
      fail("Paste some source code before analyzing.");
      sourceField.current.focus();
      return;
    }
    if (new TextEncoder().encode(source).length > config.maxSourceBytes) {
      fail(config.sourceLimitMessage);
      return;
    }
    setReport({ phase: "pending", message: "Analyzing your code…" });
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), TIMEOUT_MS);
    try {
      const response = await fetch("/analyze", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ lang: language, code: source }),
        signal: controller.signal,
      });
      const result = await response.json();
      if (!response.ok) throw new Error(result.error || "Analysis is unavailable. Please try again.");
      setReport({ phase: "success", message: resultMessage(result), result });
    } catch (error) {
      fail(errorMessage(error));
    } finally {
      clearTimeout(timeout);
    }
  }

  const { result } = report;
  return (
    <div className="demo-grid">
      <form className="editor-panel" onSubmit={analyze}>
        <div className="panel-heading">
          <label htmlFor="language">Source code</label>
          <select id="language" name="lang" aria-label="Source language" value={language}
            onChange={(event) => setLanguage(event.target.value)}>
            {config.languages.map(([name, extension]) => <option key={extension} value={extension}>{name}</option>)}
          </select>
        </div>
        <label className="visually-hidden" htmlFor="source-code">Code to analyze</label>
        <textarea id="source-code" name="code" ref={sourceField} value={source}
          onChange={(event) => setSource(event.target.value)} spellCheck={false} autoCapitalize="off"
          autoComplete="off" aria-describedby="source-note" wrap="off" />
        <div className="editor-actions">
          <button className="text-button" type="button" onClick={loadExample}
            disabled={!hasExample || pending}
            title={hasExample ? "Replace the editor with an example." : "Paste your own code for this language."}>
            Load example <Arrow>↻</Arrow>
          </button>
          <button className="button primary" type="submit" disabled={pending}>
            Analyze code <Arrow>→</Arrow>
          </button>
        </div>
        <p id="source-note" className="source-note">Up to {config.maxSourceBytes / 1024} KiB. Code is sent to this server for analysis and is not saved. For private source, <a href="#get-started">run Lizard locally</a>.</p>
      </form>
      <section className="results-panel" aria-labelledby="results-title" aria-busy={pending}>
        <div className="panel-heading"><h3 id="results-title">Your report</h3><span className="report-badge">{BADGES[report.phase]}</span></div>
        <p id="analysis-status" className={report.phase === "error" ? "error" : undefined} role="status" aria-live="polite">{report.message}</p>
        {result && (
          <div>
            <div className="result-totals">
              <div><strong>{result.function_list.length}</strong><span>functions</span></div>
              <div><strong>{result.nloc}</strong><span>lines of code</span></div>
              <div><strong>{result.token_count}</strong><span>tokens</span></div>
            </div>
            <div className="table-scroll" tabIndex={0} role="region" aria-label="Function metrics">
              <table>
                <thead><tr><th scope="col">Function</th><th scope="col"><abbr title="Cyclomatic complexity number">CCN</abbr></th><th scope="col"><abbr title="Non-comment lines of code">NLOC</abbr></th><th scope="col">Tokens</th><th scope="col">Params</th></tr></thead>
                <tbody>
                  {result.function_list.map((fn) => (
                    <tr key={`${fn.start_line}-${fn.long_name}`}>
                      <td>{fn.name}<span className="function-location">lines {fn.start_line}–{fn.end_line}</span></td>
                      {METRICS.map((field) => <td key={field}>{fn[field]}</td>)}
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        )}
        <div className="empty-report" aria-hidden="true" hidden={Boolean(result) || pending}><span>ƒ</span><p>Every function has a story.<br />Start with a few lines of code.</p></div>
      </section>
    </div>
  );
}
