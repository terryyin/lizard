"use strict";

const form = document.querySelector("#analysis-form");
const maxSourceBytes = Number(form.dataset.maxSourceBytes);
const source = document.querySelector("#source-code");
const language = document.querySelector("#language");
const submit = document.querySelector("#analyze-button");
const loadExample = document.querySelector("#load-example");
const status = document.querySelector("#analysis-status");
const badge = document.querySelector("#report-badge");
const output = document.querySelector("#report-output");
const empty = document.querySelector("#empty-report");
const panel = document.querySelector(".results-panel");
const rows = document.querySelector("#function-results");
const examples = {
  ".py": source.defaultValue,
  ".c": "int shipping_cost(int pickup, int total) {\n    if (pickup) return 0;\n    if (total >= 50) return 0;\n    return 5;\n}",
  ".js": "function shippingCost(order) {\n    if (order.isPickup) return 0;\n    if (order.total >= 50) return 0;\n    return 5;\n}",
  ".ts": "function shippingCost(pickup: boolean, total: number): number {\n    if (pickup) return 0;\n    if (total >= 50) return 0;\n    return 5;\n}",
  ".java": "class Shipping {\n    int cost(boolean pickup, int total) {\n        if (pickup) return 0;\n        if (total >= 50) return 0;\n        return 5;\n    }\n}",
  ".go": "package shipping\n\nfunc Cost(pickup bool, total int) int {\n    if pickup { return 0 }\n    if total >= 50 { return 0 }\n    return 5\n}",
  ".rs": "fn shipping_cost(pickup: bool, total: i32) -> i32 {\n    if pickup { return 0; }\n    if total >= 50 { return 0; }\n    5\n}",
  ".rb": "def shipping_cost(pickup, total)\n  return 0 if pickup\n  return 0 if total >= 50\n  5\nend",
};

function exampleAvailability() {
  loadExample.disabled = !Object.hasOwn(examples, language.value);
  loadExample.title = loadExample.disabled ? "Paste your own code for this language." : "Replace the editor with an example.";
}

language.addEventListener("change", exampleAvailability);
loadExample.addEventListener("click", () => {
  source.value = examples[language.value];
  source.focus();
});
exampleAvailability();

function showResult(result) {
  rows.replaceChildren();
  document.querySelector("#function-total").textContent = result.function_list.length;
  document.querySelector("#line-total").textContent = result.nloc;
  document.querySelector("#token-total").textContent = result.token_count;
  for (const fn of result.function_list) {
    const row = document.createElement("tr");
    const name = document.createElement("td");
    name.textContent = fn.name;
    const location = document.createElement("span");
    location.className = "function-location";
    location.textContent = `lines ${fn.start_line}–${fn.end_line}`;
    name.append(location);
    row.append(name);
    for (const field of ["cyclomatic_complexity", "nloc", "token_count", "parameter_count"]) {
      const cell = document.createElement("td");
      cell.textContent = fn[field];
      row.append(cell);
    }
    rows.append(row);
  }
  output.hidden = false;
  empty.hidden = true;
  badge.textContent = "Analysis complete";
  status.textContent = result.function_list.length
    ? `Analyzed ${result.filename}. ${result.function_list.length} function${result.function_list.length === 1 ? "" : "s"} found.`
    : "Analysis complete. No functions were found. Try a complete function or check the selected language.";
}

function showError(message) {
  status.textContent = message;
  status.classList.add("error");
  badge.textContent = "Try again";
  output.hidden = true;
  empty.hidden = false;
}

form.addEventListener("submit", async (event) => {
  event.preventDefault();
  status.classList.remove("error");
  if (!source.value.trim()) {
    showError("Paste some source code before analyzing.");
    source.focus();
    return;
  }
  if (new TextEncoder().encode(source.value).length > maxSourceBytes) {
    showError(form.dataset.sourceLimitMessage);
    return;
  }
  submit.disabled = true;
  loadExample.disabled = true;
  panel.setAttribute("aria-busy", "true");
  status.textContent = "Analyzing your code…";
  badge.textContent = "Working…";
  output.hidden = true;
  empty.hidden = true;
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 20000);
  try {
    const response = await fetch("/analyze", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ lang: language.value, code: source.value }),
      signal: controller.signal,
    });
    const result = await response.json();
    if (!response.ok) throw new Error(result.error || "Analysis is unavailable. Please try again.");
    showResult(result);
  } catch (error) {
    showError(error.name === "AbortError"
      ? "Analysis took too long. Try a smaller sample or run Lizard locally."
      : error instanceof TypeError || error instanceof SyntaxError
        ? "Unable to reach the analyzer. Please try again or run Lizard locally."
        : error.message);
  } finally {
    clearTimeout(timeout);
    submit.disabled = false;
    exampleAvailability();
    panel.setAttribute("aria-busy", "false");
  }
});
