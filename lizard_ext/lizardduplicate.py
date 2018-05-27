'''
Get Duplicated parameter lists
'''
from __future__ import print_function
from collections import deque, defaultdict
from itertools import groupby
from .default_ordered_dict import DefaultOrderedDict
from .extension_base import ExtensionBase


class CodeSnippet(object):
    def __init__(self, start_line, end_line, file_name):
        self.start_line = start_line
        self.end_line = end_line
        self.file_name = file_name

    def __str__(self):
        return "%s: %s ~ %s" % (self.file_name, self.start_line, self.end_line)

    def __repr__(self):
        return str(self)

    def fun_yet_to_come(self):
        pass

    def fun_yet_to_come2(self):
        pass


class Sequence(object):
    def __init__(self, start_line):
        self.hash = ''
        self.start_line = self.end_line = start_line

    def append_token(self, unified_token, end_line):
        self.hash += unified_token
        self.end_line = end_line

    def fun_yet_to_come(self):
        pass


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
                self._duplicate_sequences([(n, n) for n in same])
        return list(
                [(self.nodes[start], self.nodes[end]) for start, end in v]
                for v in self.duplicates)

    def _duplicate_sequences(self, sequences):
        nexts = [(s, n + 1) for s, n in sequences]

        def keyfunc(seq):
            return self.nodes[seq[1]].hash

        nexts = sorted(nexts, key=keyfunc)
        full_duplicate_stopped = False
        for _, group in groupby(nexts, keyfunc):
            group = list(group)
            if len(group) > 1:
                self._duplicate_sequences(group)
            else:
                full_duplicate_stopped = True
        if full_duplicate_stopped:
            if not self.full_inclusive_sequences(sequences):
                self.duplicates.append(sequences)

    def full_inclusive_sequences(self, sequences):
        return any(
            len(dup) == len(sequences) and dup[0][1] == sequences[0][1]
            for dup in self.duplicates)


class CodeHasher(object):
    def __init__(self, sample_size):
        self.sample_n = sample_size
        self.buffer = deque([Sequence(0) for _ in range(self.sample_n)])

    def enqueue_token(self, token, file_info_builder):
        unified_token = file_info_builder.unified_token(token)
        current_line = file_info_builder.current_line
        self.buffer.append(Sequence(current_line))
        for code_hash in self.buffer:
            code_hash.append_token(unified_token, current_line)
        return self.buffer.popleft()


class NestingStackWithUnifiedTokens(object):

    IGNORE_CONSTANT_VALUE_COUNT = 3

    def __init__(self, decorated):
        self._decorated = decorated
        self.token_register = {}
        self.constant_count = 0
        self.current_scope = set()
        self.scope_stack = [self.current_scope]

    def __getattr__(self, attr):
        return getattr(self._decorated, attr)

    def add_bare_nesting(self):
        self.current_scope = set()
        self.scope_stack.append(self.current_scope)
        self._decorated.add_bare_nesting()

    def pop_nesting(self):
        self.constant_count = 0
        print(self.token_register)
        for token in self.scope_stack.pop():
            del self.token_register[token]
        self.current_scope = self.scope_stack[-1]


    def unified_token(self, token):
        if (token[0].isdigit() or token[0] in ("'", '"')):
            if self.constant_count < self.IGNORE_CONSTANT_VALUE_COUNT:
                self.constant_count += 1
                return '10000'
        elif token[0].isalpha():
            if token not in self.token_register:
                self.token_register[token] = str(len(self.current_scope))
                self.current_scope.add(token)
            return self.token_register[token]
        return token


class LizardExtension(ExtensionBase):

    SAMPLE_SIZE = 31

    def __init__(self, context=None):
        self.duplicates = []
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        reader.context._nesting_stack = NestingStackWithUnifiedTokens(reader.context._nesting_stack)
        nodes = []
        hasher = CodeHasher(self.SAMPLE_SIZE)
        for token in tokens:
            code_hash = hasher.enqueue_token(
                    token, reader.context)
            nodes.append(code_hash)
            yield token
        nodes.append(Sequence(0))
        duplicate_finder = DuplicateFinder(nodes)
        for seq in duplicate_finder.find():
            self.add_duplicate(seq, reader.context.fileinfo.filename)

    def add_duplicate(self, sequences, file_name):
        self.duplicates.append([
            CodeSnippet(seq[0].start_line, seq[-1].end_line, file_name)
            for seq in sequences])

    def print_result(self):
        print("Duplicates")
        print("===================================")
        for dup in self.duplicates:
            print("Duplicate block:")
            print("--------------------------")
            for snippet in dup:
                print(snippet)
            print("^^^^^^^^^^^^^^^^^^^^^^^^^^")
            print("")
