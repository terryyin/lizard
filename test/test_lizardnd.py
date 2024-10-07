from lizard_ext.lizardnd import get_method, branch_coverage, print_coverage
from unittest.mock import MagicMock

def test_with_func_attribute():
    branch_coverage["has_func_branch"] = False
    branch_coverage["no_func_branch"] = False

    mock_cls = MagicMock()
    mock_method = MagicMock()
    mock_method.__func__ = MagicMock()
    setattr(mock_cls, 'method_with_func', mock_method)

    method = get_method(mock_cls, 'method_with_func')
    assert method == mock_method.__func__
    assert branch_coverage["has_func_branch"] is True
    assert branch_coverage["no_func_branch"] is False

    print_coverage()

def test_without_func_attribute():
    branch_coverage["has_func_branch"] = False
    branch_coverage["no_func_branch"] = False

    mock_cls = MagicMock()
    mock_method = MagicMock()
    setattr(mock_cls, 'method_without_func', mock_method)

    method = get_method(mock_cls, 'method_without_func')
    assert method == mock_method
    assert branch_coverage["has_func_branch"] is False
    assert branch_coverage["no_func_branch"] is True

    print_coverage()

if __name__ == "__main__":

    test_with_func_attribute()
    test_without_func_attribute()

