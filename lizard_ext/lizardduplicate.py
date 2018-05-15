'''
Get Duplicated parameter lists
'''
from collections import defaultdict, deque
from .extension_base import ExtensionBase


class CodeSnippet(object):
    def __init__(self, start_line, end_line):
        self.start_line = start_line
        self.end_line = end_line


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
        self.saved_sequences = deque()
        self.saved_hash = defaultdict(list)
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        continuous = None
        for token in tokens:
            self.saved_sequences.append(Sequence(reader.context.current_line))
            for s in self.saved_sequences:
                s.append(token, reader.context.current_line)
            if len(self.saved_sequences) > self.SAMPLE_SIZE:
                s = self.saved_sequences.popleft()
                for p in self.saved_hash[s.hash]:
                    if not continuous:
                        continuous = [CodeSnippet(p.start_line, p.end_line), CodeSnippet(s.start_line, s.start_line + 5)]
                        self.duplicates.append(continuous)
                    continuous[0].end_line = p.end_line
                    continuous[1].end_line = s.end_line
                if not self.saved_hash[s.hash]:
                    continuous = None
                self.saved_hash[s.hash].append(s)

            yield token
