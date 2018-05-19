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


class DuplicateGraphNode(object):
    def __init__(self, node, node_hash):
        self.hash = node_hash
        self.used_by = node
        self.next = None


class DuplicateGraph(object):
    def __init__(self):
        self.current = None

    def add(self,node, node_hash):
        if not self.current:
            self.current = DuplicateTreeNode(node, node_hash)
        else:
            self.current = self.current.add(node, node_hash)



class DuplicateFinder(object):
    def __init__(self, callback_add_duplicate):
        self.callback_add_duplicate = callback_add_duplicate
        self.saved_hash = defaultdict(list)
        self.active_seqs = []
        self.duplicates = {}
        self.hashed_node_indice = defaultdict(list)
        self.nodes =[]

    def find_duplicates(self, seq, seq_hash):
        return self.find_duplicates1(seq, seq_hash)
        if not self.saved_hash[seq_hash]:
            if self.active_seqs:
                self.callback_add_duplicate(self.active_seqs)
                self.active_seqs = []
        for p in self.saved_hash[seq_hash]:
            if not self.active_seqs:
                self.active_seqs = [[p], [seq]]
            else:
                self.active_seqs.append([p])
                self.active_seqs[0].append(p)
                self.active_seqs[1].append(seq)
        self.saved_hash[seq_hash].append(seq)

    def find_duplicates1(self, seq, seq_hash):
        self.nodes.append(DuplicateGraphNode(seq, seq_hash))
        self.hashed_node_indice[seq_hash].append(len(self.nodes) - 1)

    def done(self):
        duplicates = []
        for node in self.nodes:
            same = self.hashed_node_indice[node.hash]
            if len(same) > 1:
                self._duplicate_sequences(duplicates, [(n,n) for n in same])
        for v in duplicates:
            self.callback_add_duplicate([[n.used_by for n in self.nodes[start:end+1]] for (start, end) in v])

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
        self.duplicate_finder = DuplicateFinder(self.add_duplicate)
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        for token in tokens:
            s = self._push_and_pop_current_sample_queue(token, reader.context.current_line)
            self.duplicate_finder.find_duplicates(s, s.hash)
            yield token
        self.duplicate_finder.find_duplicates(Sequence(0), '')
        self.duplicate_finder.done()

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

