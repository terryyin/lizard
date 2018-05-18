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

    def chain(self,node, node_hash):
        node = DuplicateGraphNode(node, node_hash)
        self.next = node
        return node

    def until(self, node):
        yield self
        c = self
        while c != node:
            c = c.next
            yield c




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
        self.head = self.chain = DuplicateGraphNode(None, None)

    def find_duplicates(self, seq, seq_hash):
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
        self.chain = self.chain.chain(seq, seq_hash)
        self.saved_hash[seq_hash].append(self.chain)

    def done(self):
        duplicates = []
        node = self.head
        while node:
            same = self.saved_hash[node.hash]
            if len(same) > 1:
                duplicates += self._duplicate_sequences([(n,n) for n in same])
            del self.saved_hash[node.hash]
            node = node.next
        for v in duplicates:
            self.callback_add_duplicate([[n.used_by for n in l] for l in v])

    def _duplicate_sequences(self, sequences):
        duplicates = []
        nexts = [[s,n.next] for s,n in sequences]
        keyfunc = lambda x: x[1].hash
        nexts = sorted(nexts, key=keyfunc)
        stopped = False
        for _, group in groupby(nexts, keyfunc):
            group = list(group)
            if len(group) > 1:
                duplicates += self._duplicate_sequences(group)
            else:
                stopped = True
        if stopped:
            duplicates.append([node.until(to) for node, to in sequences])
        return duplicates


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
        self.duplicate_finder.find_duplicates(Sequence(0), -1)

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

