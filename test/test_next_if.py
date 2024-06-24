# test_next_if.py

from lizard_languages.code_reader import CodeStateMachine, next_if_branch_coverage, print_next_if_coverage

def test_next_if_token_not_equal():
    # Reset branch coverage
    next_if_branch_coverage["token_not_equal_branch"] = False
    next_if_branch_coverage["token_equal_branch"] = False

    # Create an instance of CodeStateMachine with a mock context (if needed)
    mock_context = {}  # Example of using an empty dictionary as mock context
    instance = CodeStateMachine(mock_context)

    # Mock the next method to avoid actual execution
    instance.next = lambda state, token: None

    # Test next_if with token not equal to expected
    instance.next_if("state1", "token1", "expected")
    assert next_if_branch_coverage["token_not_equal_branch"] is True
    assert next_if_branch_coverage["token_equal_branch"] is False

    # Print coverage after testing token not equal case
    print("After testing token not equal case:")
    print_next_if_coverage()

def test_next_if_token_equal():
    # Reset branch coverage
    next_if_branch_coverage["token_not_equal_branch"] = False
    next_if_branch_coverage["token_equal_branch"] = False

    # Create an instance of CodeStateMachine with a mock context (if needed)
    mock_context = {}  # Example of using an empty dictionary as mock context
    instance = CodeStateMachine(mock_context)

    # Mock the next method to avoid actual execution
    instance.next = lambda state, token: None

    # Test next_if with token equal to expected
    instance.next_if("state1", "expected", "expected")
    assert next_if_branch_coverage["token_not_equal_branch"] is False
    assert next_if_branch_coverage["token_equal_branch"] is True

    # Print coverage after testing token equal case
    print("After testing token equal case:")
    print_next_if_coverage()

if __name__ == "__main__":
    test_next_if_token_not_equal()
    test_next_if_token_equal()
