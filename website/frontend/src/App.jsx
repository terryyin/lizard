import Arrow from "./Arrow.jsx";
import Playground from "./Playground.jsx";

const DOCS_URL = "https://github.com/terryyin/lizard/blob/master/docs/cli.rst";
const GITHUB_URL = "https://github.com/terryyin/lizard";

function Brand({ logo, size, label }) {
  return (
    <a className="brand" href="/" aria-label={label}>
      <img src={`/website/static/img/${logo}`} width={size} height={size} alt="" />
      <span>Lizard<span className="brand-dot">.</span></span>
    </a>
  );
}

function Header() {
  return (
    <header className="site-header wrap">
      <Brand logo="logo.png" size={34} label="Lizard home" />
      <nav aria-label="Main navigation">
        <a href="#playground">Try it online</a>
        <a href={DOCS_URL}>Documentation</a>
        <a className="github-link" href={GITHUB_URL}>GitHub <Arrow /></a>
      </nav>
    </header>
  );
}

function Hero() {
  const k = (text) => <span className="code-keyword">{text}</span>;
  const n = (text) => <span className="code-number">{text}</span>;
  return (
    <section className="hero wrap" aria-labelledby="hero-title">
      <div className="hero-copy">
        <p className="eyebrow"><span className="status-dot" aria-hidden="true"></span> Open source. Built for clarity.</p>
        <h1 id="hero-title">Understand<br />your code.<br /><span>One function<br />at a time.</span></h1>
        <p className="hero-description">Find complexity before it becomes complicated. Lizard measures the shape of your source code, across the languages you use.</p>
        <div className="hero-actions">
          <a className="button primary" href="#playground">Try Lizard <Arrow>↓</Arrow></a>
          <a className="text-link" href="#get-started">Install the CLI <Arrow /></a>
        </div>
      </div>
      <div className="hero-report" aria-label="Example complexity report">
        <div className="window-bar"><span className="window-dots" aria-hidden="true">● ● ●</span><span>small functions. clear decisions.</span><Arrow /></div>
        <div className="report-code">
          <p className="code-caption">checkout.py</p>
          <pre><code>
            {k("def")} <span className="code-name">shipping_cost</span>(order):{"\n    "}
            {k("if")} order.is_pickup:{"\n        "}
            {k("return")} {n("0")}{"\n    "}
            {k("if")} order.total &gt;= {n("50")}:{"\n        "}
            {k("return")} {n("0")}{"\n    "}
            {k("return")} {n("5")}
          </code></pre>
        </div>
        <div className="example-metrics">
          <div><strong>3</strong><span>complexity</span></div>
          <div><strong>6</strong><span>lines of code</span></div>
          <div><strong>1</strong><span>parameter</span></div>
        </div>
        <p className="report-note"><Arrow>↳</Arrow> A clearer view of what’s worth a closer look.</p>
      </div>
    </section>
  );
}

function GetStarted() {
  return (
    <section id="get-started" className="get-started">
      <div className="wrap install-grid">
        <div>
          <p className="eyebrow">Your code. Your machine.</p>
          <h2>Take the whole<br />codebase for a walk.</h2>
          <p>Use Lizard in your terminal or CI. Analyze files and folders, set complexity limits, and get a report you can act on.</p>
          <a className="text-link" href={DOCS_URL}>Explore the documentation <Arrow /></a>
        </div>
        <div className="install-terminal">
          <div className="terminal-title">GET STARTED</div>
          <p><span className="terminal-comment"># Install Lizard</span><br /><code>pip install lizard</code></p>
          <p><span className="terminal-comment"># Analyze your source directory</span><br /><code>lizard src/</code></p>
          <p><span className="terminal-comment"># Flag functions above your CCN limit</span><br /><code>lizard src/ -C 15</code></p>
          <a href="https://pypi.org/project/lizard/">View on PyPI <Arrow /></a>
        </div>
      </div>
    </section>
  );
}

export default function App({ config }) {
  return (
    <>
      <a className="skip-link" href="#main">Skip to content</a>
      <Header />
      <main id="main">
        <Hero />
        <section className="language-strip" aria-label="Supported languages"><div className="wrap"><span>Many languages.<br /><strong>One clear report.</strong></span><p>C / C++ <b>·</b> Python <b>·</b> JavaScript <b>·</b> TypeScript <b>·</b> Java <b>·</b> Go <b>·</b> Rust <b>·</b> and more</p></div></section>
        <section id="playground" className="playground wrap" aria-labelledby="playground-title">
          <div className="section-heading"><div><p className="eyebrow">A little less guesswork</p><h2 id="playground-title">Meet your code.</h2></div><p>Paste a function. Pick a language.<br />See what Lizard sees.</p></div>
          <Playground config={config} />
          <dl className="metric-guide">
            <div><dt>Complexity, explained.</dt><dd>CCN counts a function’s independent paths. More decisions usually mean more paths to test.</dd></div>
            <div><dt>Count what matters.</dt><dd>NLOC excludes comments and blank lines. Tokens and parameter counts add context to the report.</dd></div>
            <div><dt>A signal to investigate.</dt><dd>Metrics guide a closer look. They work best alongside tests, reviews, and your knowledge of the code.</dd></div>
          </dl>
        </section>
        <GetStarted />
      </main>
      <footer className="wrap site-footer">
        <Brand logo="logo-small.png" size={25} />
        <p>Complex is better than complicated.</p>
        <a href={GITHUB_URL}>Made in the open <Arrow /></a>
      </footer>
    </>
  );
}
