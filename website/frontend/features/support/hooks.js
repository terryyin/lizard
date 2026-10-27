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
}
setWorldConstructor(VisitorWorld);

BeforeAll({ timeout: 30000 }, async function () {
  await server.start();
  browser = await chromium.launch();
});

Before(async function () {
  this.context = await browser.newContext();
  // Test seam for proving the suite discriminates: no analysis request reaches the server.
  if (process.env.E2E_BLOCK_ANALYSIS) await this.context.route("**/analyze", (route) => route.abort());
  this.page = await this.context.newPage();
  this.page.on("request", (request) => {
    if (new URL(request.url()).pathname === "/analyze") this.analyzeRequests += 1;
  });
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
});

AfterAll({ timeout: 30000 }, async function () {
  await browser?.close();
  await server.stop();
});
