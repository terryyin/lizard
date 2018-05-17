'''
Get Duplicated parameter lists
'''
from collections import defaultdict, deque
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


class LizardExtension(ExtensionBase):

    SAMPLE_SIZE = 21

    def __init__(self, context=None):
        self.duplicates = []
        self.saved_sequences = deque([Sequence(0)] * self.SAMPLE_SIZE)
        self.saved_hash = defaultdict(list)
        self.active_dups = []
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        for token in tokens:
            s = self._push_and_pop_current_sample_queue(token, reader.context.current_line)
            self.find_duplicates(s)

            yield token
        end_seq = Sequence(0)
        end_seq.hash = -1
        self.find_duplicates(end_seq)

    def _push_and_pop_current_sample_queue(self, token, current_line):
        self.saved_sequences.append(Sequence(current_line))
        for s in self.saved_sequences:
            s.append(token, current_line)
        return self.saved_sequences.popleft()

    def find_duplicates(self, seq):
        if not self.saved_hash[seq.hash]:
            if self.active_dups:
                self.duplicates.append(self.active_dups)
                self.active_dups = []
        for p in self.saved_hash[seq.hash]:
            if not self.active_dups:
                self.active_dups = [CodeSnippet(p.start_line), CodeSnippet(seq.start_line)]
            self.active_dups[0].end_line = p.end_line
            self.active_dups[1].end_line = seq.end_line
        self.saved_hash[seq.hash].append(seq)

