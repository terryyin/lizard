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
        return "%s:%s ~ %s" % (self.file_name, self.start_line, self.end_line)

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

    def __str__(self):
        return "<%s-%s: %s>" % (self.start_line, self.end_line, self.hash)

    def __repr__(self):
        return str(self)


class InvolvingScope(object):
    def __init__(self, boundaries, keyfunc):
        self.current_file_duplicates = []
        self.dup_starts = set()
        self.boundaries = boundaries
        self.keyfunc = keyfunc

    def same_beginning(self, same, before_same):
        new_starts = [x for x in same if x not in self.dup_starts]
        self.dup_starts |= before_same
        if len(new_starts) > 1:
            for dup in self._duplicate_sequences(new_starts):
                self.current_file_duplicates.append(dup)
                yield dup

    def fun_yet_to_come2(self):
        pass

    def _duplicate_sequences(self, same):
        sequences = [(n, n) for n in same]
        queues = deque([sequences])
        while queues:
            sequences = queues.popleft()
            if self._full_inclusive_sequences(sequences):
                continue
            nexts = [(s, n + 1) for s, n in sequences
                     if n+1 not in self.boundaries
                     and n+1 not in self.dup_starts]
            nexts = sorted(nexts, key=self.keyfunc)
            full_duplicate_stopped = len(nexts) < len(sequences)
            for _, group in groupby(nexts, self.keyfunc):
                group = list(group)
                if len(group) > 1:
                    queues.append(group)
                else:
                    full_duplicate_stopped = True
            if full_duplicate_stopped:
                yield sequences

    def _full_inclusive_sequences(self, sequences):
        return any(
            len(dup) == len(sequences) and
            dup[0][0] <= sequences[0][0] and
            dup[0][1] >= sequences[0][1]
            for dup in self.current_file_duplicates)


class DuplicateFinder(object):
    def __init__(self, nodes, boundaries, **options):
        collapse_repeat_tokens = options.get("collapse_repeat_tokens", 20)
        self.min_duplicate_tokens = options.get("min_duplicate_tokens", 0) * 2
        self.sample_size = options.get("sample_size", 0)
        self.duplicate_token_count = 0
        self.nodes = nodes
        self.boundaries = set(boundaries + [len(nodes)])
        self.hashed_node_indice = DefaultOrderedDict(list)
        for i, node_hash in enumerate(n.hash for n in nodes):
            if i in self.boundaries:
                recent = deque([''] * collapse_repeat_tokens)
            if node_hash not in recent:
                self.hashed_node_indice[node_hash].append(i)
            recent.append(node_hash)
            recent.popleft()

    def find_start_and_ends(self):
        for i, same in enumerate(self.hashed_node_indice.values()):
            if i in self.boundaries:
                scope = InvolvingScope(self.boundaries, self._keyfunc)
            before_same = set(n - self.sample_size for n in same)
            for dup in scope.same_beginning(
                    same, before_same):
                token_count = len(dup) * \
                        (dup[0][1] - dup[0][0] + self.sample_size)
                if token_count >= self.min_duplicate_tokens:
                    self.duplicate_token_count += token_count
                    yield dup

    def duplicate_rate(self):
        try:
            return self.duplicate_token_count * 1.0 / (
                    len(self.nodes) +
                    (len(self.boundaries) - 1) * (self.sample_size - 2))
        except ZeroDivisionError:
            return 0

    def unique_rate(self):
        try:
            return len(self.hashed_node_indice) / len(self.nodes)
        except ZeroDivisionError:
            return 0

    def _keyfunc(self, seq):
        try:
            return self.nodes[seq[1]].hash
        except IndexError:
            return ''


class NestingStackWithUnifiedTokens(object):

    SAMPLE_SIZE = 31
    IGNORE_CONSTANT_VALUE_COUNT = 10

    def __init__(self, decorated):
        self._decorated = decorated
        self.token_register = {}
        self.constant_count = 0
        self.current_scope = set()
        self.scope_stack = [self.current_scope]
        self.unified_tokens = []
        self.previous_token = None

    def __getattr__(self, attr):
        return getattr(self._decorated, attr)

    def add_bare_nesting(self):
        self.current_scope = set()
        self.scope_stack.append(self.current_scope)
        return self._decorated.add_bare_nesting()

    def pop_nesting(self):
        if len(self.scope_stack) > 1:
            for token in self.scope_stack.pop():
                del self.token_register[token]
            self.current_scope = self.scope_stack[-1]
        return self._decorated.pop_nesting()

    @staticmethod
    def _is_const(token):
        return token[0].isdigit() or token[0] in ("'", '"')

    def _unified_token(self, token):
        if self.previous_token in (".", "->"):
            return token
        if token == '-':
            return '+'
        if self._is_const(token):
            return '1'
        if token[0].isalpha():
            if token not in self.token_register:
                self.token_register[token] = 'v'+str(len(self.current_scope))
                self.current_scope.add(token)
            return self.token_register[token]
        return token

    def enqueue_token(self, token, current_line):
        try:
            if self._is_const(self.unified_tokens[-self.SAMPLE_SIZE]):
                self.constant_count -= 1
        except IndexError:
            pass
        if self._is_const(token):
            self.constant_count += 1
        if self.constant_count <= self.SAMPLE_SIZE / 5:
            token = self._unified_token(token)
        self.unified_tokens.append((token, current_line,))
        self.previous_token = token

    def samples(self):
        buf = deque()
        for unified_token, current_line in self.unified_tokens:
            buf.append(Sequence(current_line))
            for code_hash in buf:
                code_hash.append_token(unified_token, current_line)
            if len(buf) > self.SAMPLE_SIZE:
                yield buf.popleft()


class LizardExtension(ExtensionBase):

    def __init__(self, context=None):
        self.nodes = []
        self.fileinfos = []
        self.saved_duplicate_rate = None
        self.saved_unique_rate = None
        super(LizardExtension, self).__init__(context)

    def __call__(self, tokens, reader):
        token_unifier = reader.context.decorate_nesting_stack(
                NestingStackWithUnifiedTokens)
        for token in tokens:
            token_unifier.enqueue_token(token, reader.context.current_line)
            yield token
        reader.context.fileinfo.hash_nodes = list(token_unifier.samples())

    def cross_file_process(self, fileinfos):
        for fileinfo in fileinfos:
            self.fileinfos.append((len(self.nodes), fileinfo))
            self.nodes += fileinfo.hash_nodes
            yield fileinfo

    def get_duplicates(self, min_duplicate_tokens=70):
        boundaries = [info[0] for info in self.fileinfos]
        duplicate_finder = DuplicateFinder(
                self.nodes,
                boundaries,
                min_duplicate_tokens=min_duplicate_tokens,
                sample_size=NestingStackWithUnifiedTokens.SAMPLE_SIZE)
        for start_and_ends in duplicate_finder.find_start_and_ends():
            yield self._create_code_snippets(start_and_ends)
        self.saved_duplicate_rate = duplicate_finder.duplicate_rate()
        self.saved_unique_rate = duplicate_finder.unique_rate()

    def duplicate_rate(self):
        return self.saved_duplicate_rate

    def unique_rate(self):
        return self.saved_unique_rate

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
        print("Total duplicate rate: %.2f%%" % (self.duplicate_rate() * 100))
        print("Total unique rate: %.2f%%" % (self.unique_rate() * 100))
