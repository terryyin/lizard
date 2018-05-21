'''
Get Duplicated parameter lists
'''
from collections import OrderedDict, deque
from itertools import groupby
from .extension_base import ExtensionBase


class CodeSnippet(object):
    def __init__(self, start_line, end_line, file_name):
        self.start_line = start_line
        self.end_line = end_line
        self.file_name = file_name


class Sequence(object):
    def __init__(self, start_line):
        self.hash = ''
        self.start_line = self.end_line = start_line

    def append(self, token, current_line):
        self.hash += token
        self.end_line = current_line


class DefaultOrderedDict(OrderedDict):
    def __init__(self, default_factory=None, *a, **kw):
        OrderedDict.__init__(self, *a, **kw)
        self.default_factory = default_factory

    def __getitem__(self, key):
        try:
            return OrderedDict.__getitem__(self, key)
        except KeyError:
            return self.__missing__(key)

    def __missing__(self, key):
        if self.default_factory is None:
            raise KeyError(key)
        self[key] = value = self.default_factory()
        return value

    def __reduce__(self):
        if self.default_factory is None:
            args = tuple()
        else:
            args = self.default_factory,
        return type(self), args, None, None, self.items()

    def copy(self):
        return self.__copy__()

    def __copy__(self):
        return type(self)(self.default_factory, self)

    def __deepcopy__(self, memo):
        import copy
        return type(self)(self.default_factory,
                          copy.deepcopy(self.items()))


class DuplicateFinder(object):
    def __init__(self, nodes):
        self.duplicates = []
        self.nodes = nodes
        self.hashed_node_indice = DefaultOrderedDict(list)
        for i, node in enumerate(self.nodes):
            self.hashed_node_indice[node.hash].append(i)

    def find(self):
        for node_hash in self.hashed_node_indice:
            same = self.hashed_node_indice[node_hash]
            if len(same) > 1:
                self._duplicate_sequences([(n, n) for n in same], node_hash)
        return list(
                [(self.nodes[start], self.nodes[end]) for start, end in v]
                for v in self.duplicates)

    def _duplicate_sequences(self, sequences, current_hash):
        nexts = [(s, n + 1) for s, n in sequences]

        def keyfunc(x): return self.nodes[x[1]].hash

        nexts = sorted(nexts, key=keyfunc)
        full_duplicate_stopped = False
        for key, group in groupby(nexts, keyfunc):
            group = list(group)
            if len(group) > 1:
                self._duplicate_sequences(group, key)
            else:
                full_duplicate_stopped = True
        if full_duplicate_stopped:
            if not self.full_inclusive_sequences(sequences):
                self.duplicates.append(sequences)

    def full_inclusive_sequences(self, sequences):
        return any(
            len(dup) == len(sequences) and dup[0][1] == sequences[0][1]
            for dup in self.duplicates)


class LizardExtension(ExtensionBase):

    SAMPLE_SIZE = 21

    def __init__(self, context=None):
        self.duplicates = []
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        nodes = []
        self.saved_sequences = deque([Sequence(0) for _ in range(self.SAMPLE_SIZE)])
        for token in tokens:
            s = self._push_and_pop_current_sample_queue(
                    token, reader.context.current_line)
            nodes.append(s)
            yield token
        nodes.append(Sequence(0))
        duplicate_finder = DuplicateFinder(nodes)
        for seq in duplicate_finder.find():
            self.add_duplicate(seq, reader.context.fileinfo.filename)

    def _push_and_pop_current_sample_queue(self, token, current_line):
        self.saved_sequences.append(Sequence(current_line))
        for s in self.saved_sequences:
            s.append(token, current_line)
        return self.saved_sequences.popleft()

    def add_duplicate(self, sequences, file_name):
        self.duplicates.append([
        CodeSnippet(seq[0].start_line, seq[-1].end_line, file_name)
        for seq in sequences ])

    def print_result(self):
        print("Duplicates")
        print("===================================")
        for dup in self.duplicates:
            print("Duplicate block:")
            print("--------------------------")
            for d in dup:
                print("%s: %s ~ %s"%(d.file_name, d.start_line, d.end_line))
            print("^^^^^^^^^^^^^^^^^^^^^^^^^^")
            print("")
