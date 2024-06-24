from lizard_languages.code_reader import CodeStateMachine, next_if_branch_coverage, print_next_if_coverage

def test_next_if_token_not_equal():
    next_if_branch_coverage["token_not_equal_branch"] = False
    next_if_branch_coverage["token_equal_branch"] = False

    mock_context = {} 
    instance = CodeStateMachine(mock_context)

    instance.next = lambda state, token: None

    instance.next_if("state1", "token1", "expected")
    assert next_if_branch_coverage["token_not_equal_branch"] is True
    assert next_if_branch_coverage["token_equal_branch"] is False

    print("After testing token not equal case:")
    print_next_if_coverage()

def test_next_if_token_equal():
    next_if_branch_coverage["token_not_equal_branch"] = False
    next_if_branch_coverage["token_equal_branch"] = False

    mock_context = {}  
    instance = CodeStateMachine(mock_context)

    instance.next = lambda state, token: None

    instance.next_if("state1", "expected", "expected")
    assert next_if_branch_coverage["token_not_equal_branch"] is False
    assert next_if_branch_coverage["token_equal_branch"] is True

    print("After testing token equal case:")
    print_next_if_coverage()

if __name__ == "__main__":
    test_next_if_token_not_equal()
    test_next_if_token_equal()
