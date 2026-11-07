Feature: Choose a language and example
  Changing the language keeps the visitor's code; only loading an example replaces it.

  Scenario: Changing the language keeps the entered code until an example is loaded
    Given a visitor opens the homepage
    When the visitor selects the "JavaScript" language
    Then the code still starts with "def shipping_cost(order):"
    When the visitor loads the example
    Then the code still starts with "function shippingCost(order)"
    When the visitor analyzes the code
    Then the status announces "Analyzed .js. 1 function found."
    And the report lists function "shippingCost" with:
      | CCN | NLOC | Tokens | Params |
      | 3   | 5    | 28     | 1      |

  Scenario: Pasted code is analyzed in a language without an example
    Given a visitor opens the homepage
    When the visitor selects the "Lua" language
    Then the "Load example" button is unavailable
    When the visitor replaces the code with:
      """
      function add(a, b)
        return a + b
      end
      """
    And the visitor analyzes the code
    Then the report lists function "add" with:
      | CCN | NLOC | Params |
      | 1   | 3    | 2      |

  Scenario: Valid code without functions gives an empty result, not an error
    Given a visitor opens the homepage
    When the visitor replaces the code with:
      """
      answer = 42
      """
    And the visitor analyzes the code
    Then the status announces "Analysis complete. No functions were found. Try a complete function or check the selected language."
    And the report shows 0 functions, 1 line of code and 3 tokens
