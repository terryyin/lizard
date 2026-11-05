export default {
  paths: ["features/**/*.feature"],
  import: ["features/support/**/*.js", "features/step_definitions/**/*.js"],
  format: ["progress", "html:reports/cucumber.html"],
  parallel: 0,
};
