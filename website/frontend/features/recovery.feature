Feature: Recover from rejected or unavailable analysis
  Feedback is actionable, the code stays editable and a retry succeeds.

  Scenario: Empty code is rejected and a retry succeeds
    Given a visitor opens the homepage
    When the visitor clears the code
    And the visitor analyzes the code
    Then the status announces "Paste some source code before analyzing."
    And the code field is focused and editable
    When the visitor replaces the code with:
      """
      def answer():
          return 42
      """
    And the visitor analyzes the code
    Then the status announces "Analyzed .py. 1 function found."

  Scenario: Code over 64 KiB of UTF-8 is rejected and a retry succeeds
    Given a visitor opens the homepage
    When the visitor enters 32769 copies of "é"
    And the visitor analyzes the code
    Then the status announces "The online demo accepts up to 64 KiB of source code. Use the local CLI for larger files."
    And the code field is editable
    When the visitor enters 32768 copies of "é"
    And the visitor analyzes the code
    Then the status announces "Analysis complete. No functions were found. Try a complete function or check the selected language."

  Scenario: Progress is announced and a second submission is prevented
    Given a visitor opens the homepage
    And the analyzer stops responding
    When the visitor analyzes the code
    Then the status announces "Analyzing your code…"
    And the report is marked busy
    And the "Analyze code" button is unavailable
    When the analyzer responds again
    Then the status announces "Analyzed .py. 1 function found."
    And exactly 1 analysis request was sent

  Scenario: Unavailable analysis hides stale results and recovers after a retry
    Given a visitor opens the homepage
    When the visitor analyzes the code
    Then the status announces "Analyzed .py. 1 function found."
    When the analyzer goes offline
    And the visitor analyzes the code
    Then the status announces "Unable to reach the analyzer. Please try again or run Lizard locally."
    And the function metrics are hidden
    And the code still starts with "def shipping_cost(order):"
    And a "run Lizard locally" link leads to "#get-started"
    When the analyzer comes back online
    And the visitor analyzes the code
    Then the status announces "Analyzed .py. 1 function found."

  Scenario: A stalled analyzer stops the wait after 20 seconds
    Given a visitor opens the homepage
    And the analyzer stops responding
    When the visitor analyzes the code
    Then the status announces "Analysis took too long. Try a smaller sample or run Lizard locally." within 25 seconds
    And the code still starts with "def shipping_cost(order):"
    When the analyzer responds again
    And the visitor analyzes the code
    Then the status announces "Analyzed .py. 1 function found."
