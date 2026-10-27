Feature: Use the demo by keyboard and on a narrow screen

  Scenario: A keyboard user reaches the demo and analyzes the code
    Given a visitor opens the homepage
    When the visitor tabs to the "Code to analyze" field
    And the visitor tabs to the "Analyze code" button and presses Enter
    Then the status announces "Analyzed .py. 1 function found."

  Scenario: The demo works on a narrow screen
    Given a visitor opens the homepage on a 375 pixel wide screen
    Then the page does not scroll sideways
    And the code field and "Analyze code" button fit the screen
    When the visitor analyzes the code
    Then the status announces "Analyzed .py. 1 function found."
    And the page does not scroll sideways

  Scenario: Keyboard focus on the demo controls is visible
    Given a visitor opens the homepage
    When the visitor tabs to the "Code to analyze" field
    Then the focused control shows a visible focus indicator
    When the visitor tabs to the "Load example" button
    Then the focused control shows a visible focus indicator
    When the visitor tabs to the "Analyze code" button
    Then the focused control shows a visible focus indicator
