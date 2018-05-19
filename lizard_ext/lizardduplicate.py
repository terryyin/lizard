'''
Get Duplicated parameter lists
'''
from collections import defaultdict, deque, Counter
from itertools import groupby
from .extension_base import ExtensionBase


class CodeSnippet(object):
    def __init__(self, start_line):
        self.start_line = start_line
        self.end_line = start_line


class Sequence(object):
    def __init__(self, start_line):
        self.hash = ''
        self.start_line = self.end_line = start_line

    def append(self, token, current_line):
        self.hash += token
        self.end_line = current_line

    def __eq__(self, other):
        return self.hash == other.hash


class DuplicateFinder(object):
    def __init__(self):
        self.saved_hash = defaultdict(list)
        self.active_seqs = []
        self.duplicates = {}
        self.hashed_node_indice = defaultdict(list)
        self.nodes = []

    def find_duplicates(self, node):
        self.nodes.append(node)
        self.hashed_node_indice[node.hash].append(len(self.nodes) - 1)

    def done(self):
        duplicates = []
        for node in self.nodes:
            same = self.hashed_node_indice[node.hash]
            if len(same) > 1:
                self._duplicate_sequences(duplicates, [(n, n) for n in same])
        return [[(self.nodes[start], self.nodes[end]) for start, end in v] for v in duplicates]

    def _duplicate_sequences(self, results, sequences):
        if len(sequences) == len(self.hashed_node_indice[self.nodes[sequences[0][1]].hash]):
            del self.hashed_node_indice[self.nodes[sequences[0][1]].hash]
        nexts = [(s,n+1) for s,n in sequences]
        keyfunc = lambda x: self.nodes[x[1]].hash
        nexts = sorted(nexts, key=keyfunc)
        full_duplicate_stopped = False
        for _, group in groupby(nexts, keyfunc):
            group = list(group)
            if len(group) > 1:
                self._duplicate_sequences(results, group)
            else:
                full_duplicate_stopped = True
        if full_duplicate_stopped:
            if not self.full_inclusive_sequences(results, sequences):
                results.append(sequences)

    def full_inclusive_sequences(self, existing, sequences):
        return any(
            len(dup) == len(sequences) and dup[0][1]==sequences[0][1]
                for dup in existing)


class LizardExtension(ExtensionBase):

    SAMPLE_SIZE = 21

    def __init__(self, context=None):
        self.duplicates = []
        self.saved_sequences = deque([Sequence(0)] * self.SAMPLE_SIZE)
        self.duplicate_finder = DuplicateFinder()
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        for token in tokens:
            s = self._push_and_pop_current_sample_queue(token, reader.context.current_line)
            self.duplicate_finder.find_duplicates(s)
            yield token
        self.duplicate_finder.find_duplicates(Sequence(0))
        for seq in self.duplicate_finder.done():
            self.add_duplicate(seq)

    def _push_and_pop_current_sample_queue(self, token, current_line):
        self.saved_sequences.append(Sequence(current_line))
        for s in self.saved_sequences:
            s.append(token, current_line)
        return self.saved_sequences.popleft()

    def add_duplicate(self, sequences):
        dup1 = CodeSnippet(sequences[0][0].start_line)
        dup1.end_line = sequences[0][-1].end_line
        dup2 = CodeSnippet(sequences[1][0].start_line)
        dup2.end_line = sequences[1][-1].end_line
        self.duplicates.append([dup1, dup2])

