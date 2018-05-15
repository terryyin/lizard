'''
Get Duplicated parameter lists
'''
from collections import Counter
from .extension_base import ExtensionBase


class Duplicate(object):
    def __init__(self, start_line, end_line):
        self.start_line = start_line
        self.end_line = end_line


class Sequence(object):
    def __init__(self):
        self.seq = ''

    def append(self, token):
        self.seq += token

    def __eq__(self, other):
        return self.seq == other.seq


class LizardExtension(ExtensionBase):

    SAMPLE_SIZE = 21

    def __init__(self, context=None):
        self.duplicates = []
        self.saved_sequences = []
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        continuous = False
        for token in tokens:
            self.saved_sequences.append(Sequence())
            for s in self.saved_sequences[-self.SAMPLE_SIZE:]:
                s.append(token)
            for p in self._duplicates():
                if not continuous:
                    self.duplicates.append([Duplicate(1, 6), 1])
                    continuous = True
            if not self._duplicates():
                continuous = False

            yield token

    def _duplicates(self):
        return [p for p in (self.saved_sequences[:-self.SAMPLE_SIZE])
            if self.saved_sequences[-self.SAMPLE_SIZE] == p]
