import pytest
from lizard_ext.default_ordered_dict import DefaultOrderedDict, print_coverage

def test_default_ordered_dict_with_default_factory():
    d = DefaultOrderedDict(int)
    assert d["missing_key"] == 0  # Should trigger __missing__ with default_factory

def test_default_ordered_dict_without_default_factory():
    d = DefaultOrderedDict()
    with pytest.raises(KeyError):
        d["missing_key"]  # Should trigger __missing__ and raise KeyError

def test_reduce_with_default_factory():
    d = DefaultOrderedDict(int)
    assert d.__reduce__() == (DefaultOrderedDict, (int,), None, None, list(d.items()))

def test_reduce_without_default_factory():
    d = DefaultOrderedDict()
    assert d.__reduce__() == (DefaultOrderedDict, tuple(), None, None, list(d.items()))

if __name__ == "__main__":
    test_default_ordered_dict_with_default_factory()
    test_default_ordered_dict_without_default_factory()
    test_reduce_with_default_factory()
    test_reduce_without_default_factory()
    print_coverage()
