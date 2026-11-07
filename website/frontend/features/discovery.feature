Feature: Discover Lizard and analyze the sample
  Visitors find how to install Lizard and see real metrics for the sample code.

  Scenario: Installation and documentation are findable
    Given a visitor opens the homepage
    Then a "Install the CLI" link leads to "#get-started"
    And a "Documentation" link leads to "https://github.com/terryyin/lizard/blob/master/docs/cli.rst"
    And the page shows the install command "pip install lizard"

  Scenario: The Python sample reports its metrics from the analyzer
    Given a visitor opens the homepage
    When the visitor analyzes the code
    Then the status announces "Analyzed .py. 1 function found."
    And the report shows 1 function, 6 lines of code and 24 tokens
    And the report lists function "shipping_cost" with:
      | CCN | NLOC | Tokens | Params |
      | 3   | 6    | 23     | 1      |
