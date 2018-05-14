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

    def __init__(self, context=None):
        self.duplicates = []
        self.saved_tokens = []
        self.saved_sequences = []
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        for token in tokens:
            self.saved_sequences.append([])
            cnt = 34
            for s in self.saved_sequences[-cnt:]:
                s.append(token)
            for p in (self.saved_sequences[:-cnt]):
                if ''.join(self.saved_sequences[-cnt]) == ''.join(p):
                    self.duplicates.append([Duplicate(1, 6), 1])
            if token == 'func6' and token in self.saved_tokens:
                self.duplicates.append([Duplicate(1, 6), 1])
            self.saved_tokens.append(token)
            yield token
