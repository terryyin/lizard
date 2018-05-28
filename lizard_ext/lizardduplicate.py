'''
Get Duplicated parameter lists
'''
from __future__ import print_function
from collections import deque
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

    def find_start_and_ends(self):
        for node_hash in self.hashed_node_indice:
            same = self.hashed_node_indice[node_hash]
            if len(same) > 1:
                self._duplicate_sequences([(n, n) for n in same])
        return self.duplicates

    def _duplicate_sequences(self, sequences):

        def keyfunc(seq):
            try:
                return self.nodes[seq[1]].hash
            except IndexError:
                return ''

        queues = deque([sequences])
        while queues:
            sequences = queues.popleft()
            nexts = [(s, n + 1) for s, n in sequences]
            nexts = sorted(nexts, key=keyfunc)
            full_duplicate_stopped = False
            for _, group in groupby(nexts, keyfunc):
                group = list(group)
                if len(group) > 1:
                    queues.append(group)
                else:
                    full_duplicate_stopped = True
            if full_duplicate_stopped:
                if not self.full_inclusive_sequences(sequences):
                    self.duplicates.append(sequences)

    def full_inclusive_sequences(self, sequences):
        return any(
            len(dup) == len(sequences) and dup[0][1] == sequences[0][1]
            for dup in self.duplicates)


class NestingStackWithUnifiedTokens(object):

    SAMPLE_SIZE = 31
    IGNORE_CONSTANT_VALUE_COUNT = 3

    def __init__(self, decorated):
        self._decorated = decorated
        self.token_register = {}
        self.constant_count = 0
        self.current_scope = set()
        self.scope_stack = [self.current_scope]
        self.buffer = deque([Sequence(0) for _ in range(self.SAMPLE_SIZE)])

    def __getattr__(self, attr):
        return getattr(self._decorated, attr)

    def add_bare_nesting(self):
        self.current_scope = set()
        self.scope_stack.append(self.current_scope)
        return self._decorated.add_bare_nesting()

    def pop_nesting(self):
        self.constant_count = 0
        if len(self.scope_stack) > 1:
            for token in self.scope_stack.pop():
                del self.token_register[token]
            self.current_scope = self.scope_stack[-1]
        return self._decorated.pop_nesting()

    def _unified_token(self, token):
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

    def enqueue_token(self, token, current_line):
        unified_token = self._unified_token(token)
        self.buffer.append(Sequence(current_line))
        for code_hash in self.buffer:
            code_hash.append_token(unified_token, current_line)
        return self.buffer.popleft()


class LizardExtension(ExtensionBase):

    def __init__(self, context=None):
        self.nodes = []
        self.fileinfos = []
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        token_unifier = reader.context.decorate_nesting_stack(
                NestingStackWithUnifiedTokens)
        reader.context.fileinfo.hash_nodes = nodes = []
        for token in tokens:
            code_hash = token_unifier.enqueue_token(
                    token, reader.context.current_line)
            nodes.append(code_hash)
            yield token
        nodes.append(Sequence(0))

    def reduce(self, fileinfo):
        self.fileinfos.append((len(self.nodes), fileinfo))
        self.nodes += fileinfo.hash_nodes

    def get_duplicates(self):
        duplicate_finder = DuplicateFinder(self.nodes)
        duplicates = []
        for start_and_ends in duplicate_finder.find_start_and_ends():
            duplicates.append(self._create_code_snippets(start_and_ends))
        return duplicates

    def _create_code_snippets(self, start_and_ends):
        return [
            CodeSnippet(
                self.nodes[start].start_line,
                self.nodes[end].end_line,
                self._get_fileinfo_by_token_index(start).filename)
            for start, end in start_and_ends]

    def _get_fileinfo_by_token_index(self, index):
        last_fileinfo = (-1, None)
        for fileinfo in self.fileinfos:
            if fileinfo[0] > index:
                break
            last_fileinfo = fileinfo
        return last_fileinfo[1]

    def print_result(self):
        print("Duplicates")
        print("===================================")
        for dup in self.get_duplicates():
            print("Duplicate block:")
            print("--------------------------")
            for snippet in dup:
                print(snippet)
            print("^^^^^^^^^^^^^^^^^^^^^^^^^^")
            print("")
