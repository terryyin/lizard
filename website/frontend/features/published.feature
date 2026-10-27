Feature: Serve the built page at its published addresses
  The page comes from the production build, keeps the published logo and
  favicon URLs and points visitors without JavaScript to the command-line tool.

  Scenario: The logo and favicon keep their published URLs
    Given a visitor opens the homepage
    Then the page uses the favicon "/website/static/favicon.ico"
    And the page shows the logo "/website/static/img/logo.png"
    And the page shows the logo "/website/static/img/logo-small.png"
    And "/favicon.ico" is served as an image

  Scenario: The page runs from the built bundle, not the retired assets
    Given a visitor opens the homepage
    Then every script and stylesheet the page loads comes from "/website/static/dist/"
    And "/website/static/demo.js" is not served
    And "/website/static/site.css" is not served

  Scenario: Visitors without JavaScript are pointed to the command-line tool
    Given a visitor without JavaScript opens the homepage
    Then a "use the command-line tool" link leads to "https://github.com/terryyin/lizard/blob/master/docs/cli.rst"
    And the page shows the install command "pip install lizard"
