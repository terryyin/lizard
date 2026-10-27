import assert from "node:assert/strict";
import { mkdir } from "node:fs/promises";
import {
  After, AfterAll, Before, BeforeAll, setDefaultTimeout, setWorldConstructor, Status, World,
} from "@cucumber/cucumber";
import { chromium } from "playwright";
import { LizardServer } from "./server.js";

setDefaultTimeout(15000);

const server = new LizardServer();
let browser;

process.once("exit", () => server.killNow());
for (const signal of ["SIGINT", "SIGTERM"]) {
  process.once(signal, () => process.exit(130)); // the exit handler kills the server
}

class VisitorWorld extends World {
  server = server;
  analyzeRequests = 0;
  requests = [];
  browserProblems = [];

  /** A fresh browser context and page; replaces any context the scenario already opened. */
  async openContext(options = {}) {
    await this.context?.close();
    this.context = await browser.newContext(options);
    // Test seam for proving the suite discriminates: no analysis request reaches the server.
    if (process.env.E2E_BLOCK_ANALYSIS) await this.context.route("**/analyze", (route) => route.abort());
    this.page = await this.context.newPage();
    this.page.on("request", (request) => {
      this.requests.push(request);
      if (new URL(request.url()).pathname === "/analyze") this.analyzeRequests += 1;
    });
    this.page.on("pageerror", (error) => this.browserProblems.push(error.message));
    this.page.on("console", (message) => {
      if (/Content Security Policy/i.test(message.text())) this.browserProblems.push(message.text());
    });
  }
}
setWorldConstructor(VisitorWorld);

BeforeAll({ timeout: 30000 }, async function () {
  await server.start();
  browser = await chromium.launch();
});

Before(async function () {
  await this.openContext();
});

After({ timeout: 30000 }, async function ({ pickle, result }) {
  try {
    if (result?.status === Status.FAILED && this.page) {
      await mkdir("reports", { recursive: true });
      const path = `reports/${pickle.name.replace(/[^\w-]+/g, "-")}.png`;
      const log = `Server log tail:\n${server.logTail()}`;
      this.attach(await this.page.screenshot({ path, fullPage: true }), "image/png");
      this.attach(log, "text/plain");
      console.error(`\nFailure screenshot: ${path}\n${log}`);
    }
  } finally {
    await this.context?.close();
    server.resume();
    if (!server.running) await server.start(); // restore a server a scenario took offline
  }
  assert.deepEqual(this.browserProblems, [], "page errors or Content Security Policy violations");
});

AfterAll({ timeout: 30000 }, async function () {
  await browser?.close();
  await server.stop();
});
