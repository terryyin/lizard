import assert from "node:assert/strict";
import { Given, Then, When } from "@cucumber/cucumber";
import { expect } from "playwright/test";

const sourceField = (page) => page.getByRole("textbox", { name: "Code to analyze" });
const languageField = (page) => page.getByRole("combobox", { name: "Source language" });
const analyzeButton = (page) => page.getByRole("button", { name: "Analyze code" });
const loadExampleButton = (page) => page.getByRole("button", { name: "Load example" });
const report = (page) => page.getByRole("region", { name: "Your report" });
const metricsTable = (page) => page.getByRole("region", { name: "Function metrics" });

function reportTotal(page, label) {
  return report(page).locator("div").filter({ has: page.getByText(label, { exact: true }) }).last()
    .locator("strong");
}

const isFocused = (locator) => locator.evaluate((element) => element === document.activeElement);

Given("a visitor opens the homepage", async function () {
  await this.page.goto(this.server.url);
});

Given("a visitor without JavaScript opens the homepage", async function () {
  await this.openContext({ javaScriptEnabled: false });
  await this.page.goto(this.server.url);
});

Given("a visitor opens the homepage on a {int} pixel wide screen", async function (width) {
  await this.page.setViewportSize({ width, height: 800 });
  await this.page.goto(this.server.url);
});

When("the visitor analyzes the code", async function () {
  await analyzeButton(this.page).click();
});

When("the visitor selects the {string} language", async function (language) {
  await languageField(this.page).selectOption({ label: language });
});

When("the visitor loads the example", async function () {
  await loadExampleButton(this.page).click();
});

When("the visitor replaces the code with:", async function (code) {
  await sourceField(this.page).fill(code);
});

When("the visitor clears the code", async function () {
  await sourceField(this.page).fill("");
});

When("the visitor enters {int} copies of {string}", async function (count, text) {
  await sourceField(this.page).fill(text.repeat(count));
});

async function tabTo(page, target, presses) {
  for (let pressed = 0; pressed < presses; pressed += 1) {
    await page.keyboard.press("Tab");
    if (await isFocused(target)) return;
  }
  assert.fail(`${target} was not reached within ${presses} Tab presses`);
}

When("the visitor tabs to the {string} field", async function (name) {
  await tabTo(this.page, this.page.getByRole("textbox", { name }), 40);
});

When("the visitor tabs to the {string} button", async function (name) {
  await tabTo(this.page, this.page.getByRole("button", { name }), 5);
});

When("the visitor tabs to the {string} button and presses Enter", async function (name) {
  await tabTo(this.page, this.page.getByRole("button", { name }), 5);
  await this.page.keyboard.press("Enter");
});

When("the analyzer stops responding", function () {
  this.server.pause();
});

When("the analyzer responds again", function () {
  this.server.resume();
});

When("the analyzer goes offline", async function () {
  await this.server.stop();
});

When("the analyzer comes back online", { timeout: 30000 }, async function () {
  await this.server.start();
});

Then("the report shows {int} function(s), {int} line(s) of code and {int} token(s)",
  async function (functions, lines, tokens) {
    await expect(reportTotal(this.page, "functions")).toHaveText(String(functions));
    await expect(reportTotal(this.page, "lines of code")).toHaveText(String(lines));
    await expect(reportTotal(this.page, "tokens")).toHaveText(String(tokens));
  });

Then("the report lists function {string} with:", async function (name, dataTable) {
  const table = metricsTable(this.page);
  const row = table.getByRole("row").filter({ hasText: name });
  await expect(row).toHaveCount(1);
  const headers = await table.getByRole("columnheader").allInnerTexts();
  const cells = row.getByRole("cell");
  for (const [column, value] of Object.entries(dataTable.hashes()[0])) {
    const index = headers.indexOf(column);
    assert.notEqual(index, -1, `no "${column}" column in ${headers.join(", ")}`);
    await expect(cells.nth(index)).toHaveText(value);
  }
});

Then("the function metrics are hidden", async function () {
  await expect(metricsTable(this.page)).toBeHidden();
});

Then("the status announces {string}", async function (message) {
  await expect(this.page.getByRole("status")).toHaveText(message);
});

Then("the status announces {string} within {int} seconds", { timeout: 40000 }, async function (message, seconds) {
  await expect(this.page.getByRole("status")).toHaveText(message, { timeout: seconds * 1000 });
});

Then("the report is marked busy", async function () {
  await expect(report(this.page)).toHaveAttribute("aria-busy", "true");
});

Then("exactly {int} analysis request(s) was/were sent", function (count) {
  assert.equal(this.analyzeRequests, count);
});

Then("the code still starts with {string}", async function (start) {
  await expect(sourceField(this.page)).toHaveValue(new RegExp(`^${RegExp.escape(start)}`));
});

Then("the code field is focused and editable", async function () {
  await expect(sourceField(this.page)).toBeFocused();
  await expect(sourceField(this.page)).toBeEditable();
});

Then("the code field is editable", async function () {
  await expect(sourceField(this.page)).toBeEditable();
});

Then("the {string} button is unavailable", async function (name) {
  await expect(this.page.getByRole("button", { name })).toBeDisabled();
});

Then("a {string} link leads to {string}", async function (name, href) {
  const link = this.page.getByRole("link", { name }).first();
  await expect(link).toBeVisible();
  await expect(link).toHaveAttribute("href", href);
});

Then("the page shows the install command {string}", async function (command) {
  await expect(this.page.getByText(command, { exact: true })).toBeVisible();
});

Then("the page does not scroll sideways", async function () {
  const overflow = await this.page.evaluate(
    () => document.documentElement.scrollWidth - document.documentElement.clientWidth,
  );
  assert.ok(overflow <= 0, `page is ${overflow}px wider than the viewport`);
});

Then("the code field and {string} button fit the screen", async function (name) {
  const width = this.page.viewportSize().width;
  for (const locator of [sourceField(this.page), this.page.getByRole("button", { name })]) {
    await locator.scrollIntoViewIfNeeded();
    const box = await locator.boundingBox();
    assert.ok(box && box.x >= 0 && box.x + box.width <= width, `${JSON.stringify(box)} exceeds ${width}px`);
  }
});

Then("the focused control shows a visible focus indicator", async function () {
  const focus = await this.page.evaluate(() => {
    const style = getComputedStyle(document.activeElement);
    return {
      focusVisible: document.activeElement.matches(":focus-visible"),
      outline: style.outlineStyle === "none" ? 0 : parseFloat(style.outlineWidth),
      boxShadow: style.boxShadow,
    };
  });
  assert.ok(focus.focusVisible, "the focused control does not match :focus-visible");
  // At least 2 CSS px, thicker than the browser's default ring, so the site's own style is observed.
  assert.ok(focus.outline >= 2 || focus.boxShadow !== "none", `no visible focus indicator: ${JSON.stringify(focus)}`);
});

async function assertImage(page, path) {
  const response = await page.request.get(new URL(path, page.url()).href);
  assert.equal(response.status(), 200, `${path} answered ${response.status()}`);
  assert.match(response.headers()["content-type"], /^image\//, `${path} is not an image`);
  assert.ok((await response.body()).length > 0, `${path} is empty`);
}

Then("the page uses the favicon {string}", async function (path) {
  await expect(this.page.locator('link[rel="icon"]')).toHaveAttribute("href", path);
  await assertImage(this.page, path);
});

Then("the page shows the logo {string}", async function (path) {
  await expect(this.page.locator(`img[src="${path}"]`)).toBeVisible();
  await assertImage(this.page, path);
});

Then("{string} is served as an image", async function (path) {
  await assertImage(this.page, path);
});

Then("every script and stylesheet the page loads comes from {string}", async function (prefix) {
  const assets = this.requests.filter((request) => ["script", "stylesheet"].includes(request.resourceType()));
  assert.deepEqual([...new Set(assets.map((request) => request.resourceType()))].sort(), ["script", "stylesheet"]);
  for (const request of assets) {
    assert.ok(new URL(request.url()).pathname.startsWith(prefix), `${request.url()} is outside ${prefix}`);
    assert.equal((await request.response())?.status(), 200, `${request.url()} did not load`);
  }
});

Then("{string} is not served", async function (path) {
  const response = await this.page.request.get(new URL(path, this.page.url()).href);
  assert.equal(response.status(), 404, `${path} answered ${response.status()}`);
});
