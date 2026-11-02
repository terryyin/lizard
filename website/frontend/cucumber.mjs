export default {
  paths: ["features/**/*.feature"],
  import: ["features/support/**/*.js", "features/step_definitions/**/*.js"],
  format: ["progress", "html:reports/cucumber.html"],
  plugin: process.env.RECORD_CUCUMBER_PROOF === "1" ? ["./proof-plugin.js"] : [],
  parallel: 0,
};
