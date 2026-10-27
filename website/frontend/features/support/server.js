import { spawn } from "node:child_process";
import { createServer } from "node:net";
import { fileURLToPath } from "node:url";

const repositoryRoot = fileURLToPath(new URL("../../../../", import.meta.url));
const READY_TIMEOUT_MS = 15000;
const STOP_TIMEOUT_MS = 5000;

function freePort() {
  return new Promise((resolve, reject) => {
    const probe = createServer();
    probe.once("error", reject);
    probe.listen(0, "127.0.0.1", () => {
      const { port } = probe.address();
      probe.close(() => resolve(port));
    });
  });
}

const delay = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

/** The real Flask app (`index:app`) as a child process owned by the test run. */
export class LizardServer {
  #child = null;
  #log = [];

  get running() { return this.#child !== null; }

  async start() {
    this.port ??= await freePort();
    this.url = `http://127.0.0.1:${this.port}`;
    const child = spawn(
      "python",
      ["-m", "flask", "--app", "index", "run", "--host", "127.0.0.1", "--port", String(this.port)],
      { cwd: repositoryRoot, stdio: ["ignore", "pipe", "pipe"] },
    );
    this.#child = child;
    const record = (chunk) => { this.#log.push(...chunk.toString().split("\n").filter(Boolean)); };
    child.stdout.on("data", record);
    child.stderr.on("data", record);
    child.once("exit", (code, signal) => {
      this.#log.push(`[server exited: code=${code} signal=${signal}]`);
      if (this.#child === child) this.#child = null;
    });
    const deadline = Date.now() + READY_TIMEOUT_MS;
    while (Date.now() < deadline) {
      if (this.#child !== child) break;
      try {
        if ((await fetch(this.url, { signal: AbortSignal.timeout(1000) })).ok) return;
      } catch {
        // not listening yet
      }
      await delay(100);
    }
    await this.stop();
    throw new Error(`Lizard server did not become ready at ${this.url}\n${this.logTail()}`);
  }

  async stop() {
    const child = this.#child;
    if (!child) return;
    const exited = new Promise((resolve) => child.once("exit", resolve));
    child.kill("SIGCONT");
    child.kill("SIGTERM");
    const timer = setTimeout(() => child.kill("SIGKILL"), STOP_TIMEOUT_MS);
    await exited;
    clearTimeout(timer);
  }

  /** Stall the owned server: it keeps its socket but answers nothing until resumed. */
  pause() { this.#child?.kill("SIGSTOP"); }

  resume() { this.#child?.kill("SIGCONT"); }

  killNow() { this.#child?.kill("SIGKILL"); }

  logTail(lines = 20) { return this.#log.slice(-lines).join("\n"); }
}
