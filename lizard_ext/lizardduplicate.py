'''
Get Duplicated parameter lists
'''
from collections import Counter
from .extension_base import ExtensionBase


class Duplicate(object):
    def __init__(self, start_line, end_line):
        self.start_line = start_line
        self.end_line = end_line


class LizardExtension(ExtensionBase):

    SAMPLE_SIZE = 31

    def __init__(self, context=None):
        self.duplicates = []
        self.saved_sequences = []
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        for token in tokens:
            self.saved_sequences.append([])
            for s in self.saved_sequences[-self.SAMPLE_SIZE:]:
                s.append(token)
            for p in self._duplicates():
                self.duplicates=[[Duplicate(1, 6), 1]]
            yield token

    def _duplicates(self):
        return [p for p in (self.saved_sequences[:-self.SAMPLE_SIZE])
            if ''.join(self.saved_sequences[-self.SAMPLE_SIZE]) == ''.join(p)]
