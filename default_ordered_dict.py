from collections import OrderedDict

branch_coverage = {}

def log_branch(branch_id):
    if branch_id not in branch_coverage:
        branch_coverage[branch_id] = 0
    branch_coverage[branch_id] += 1

class DefaultOrderedDict(OrderedDict):
    def __init__(self, default_factory=None):
        OrderedDict.__init__(self)
        self.default_factory = default_factory

    def __getitem__(self, key):
        try:
            return OrderedDict.__getitem__(self, key)
        except KeyError:
            log_branch(16)  # Branch ID: 16
            return self.__missing__(key)

    def __missing__(self, key):
        if self.default_factory is None:
            log_branch(21)  # Branch ID: 21
            raise KeyError(key)
        self[key] = value = self.default_factory()
        return value

    def __reduce__(self):
        if self.default_factory is None:
            log_branch(26)  # Branch ID: 26
            args = tuple()
            log_branch(27)  # Branch ID: 27
        else:
            log_branch(29)  # Branch ID: 29
            args = (self.default_factory,)
        log_branch(30)  # Branch ID: 30
        return type(self), args, None, None, self.items()

def print_coverage():
    for branch_id, count in branch_coverage.items():
        print(f"Branch {branch_id}: executed {count} times")

if __name__ == "__main__":
    d = DefaultOrderedDict(int)
    print(d["missing_key"])  # Should trigger __missing__ and default_factory
    d2 = DefaultOrderedDict()
    try:
        print(d2["missing_key"])  # Should raise KeyError
    except KeyError:
        pass
    print_coverage()
