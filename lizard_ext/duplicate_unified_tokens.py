'''
Unify tokens for duplicate detection and track data density in the sample window.
'''
from collections import deque, namedtuple

UnifiedTokenRecord = namedtuple(
    'UnifiedTokenRecord', 'token line counts_as_data')


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


class NestingStackWithUnifiedTokens(object):

    SAMPLE_SIZE = 31
    IGNORE_CONSTANT_VALUE_COUNT = 10

    def __init__(self, decorated):
        self._decorated = decorated
        self.token_register = {}
        self.data_count = 0
        self.current_scope = set()
        self.scope_stack = [self.current_scope]
        self.unified_tokens = []
        self.previous_token = None
        self.token_before_previous = None
        self.expect_designator_value = False

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

    def _is_designator(self, token):
        # C designated initializer: {.name=...} or {..., .name=...}
        return (
            token[0].isalpha() and
            self.previous_token == "." and
            self.token_before_previous in ("{", ",")
        )

    def _counts_as_data(self, token, is_designator):
        # Treat a whole designated field as data so lookup tables saturate the
        # constant-density window the same way positional initializers do.
        return (
            self._is_const(token) or
            is_designator or
            self.expect_designator_value
        )

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
            if self.unified_tokens[-self.SAMPLE_SIZE].counts_as_data:
                self.data_count -= 1
        except IndexError:
            pass
        is_designator = self._is_designator(token)
        counts_as_data = self._counts_as_data(token, is_designator)
        if counts_as_data:
            self.data_count += 1
        if self.data_count <= self.SAMPLE_SIZE / 5:
            unified = self._unified_token(token)
        else:
            unified = token
        self.unified_tokens.append(
            UnifiedTokenRecord(unified, current_line, counts_as_data))
        if is_designator:
            self.expect_designator_value = True
        elif self.expect_designator_value and token != "=":
            self.expect_designator_value = False
        self.token_before_previous = self.previous_token
        self.previous_token = token

    def samples(self):
        buf = deque()
        for record in self.unified_tokens:
            buf.append(Sequence(record.line))
            for code_hash in buf:
                code_hash.append_token(record.token, record.line)
            if len(buf) > self.SAMPLE_SIZE:
                yield buf.popleft()
