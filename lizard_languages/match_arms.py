'''`match` / `try` / `function` arm counting shared by the ML-family
readers (OCaml, F#).'''


class _ArmScope:
    '''An open `match` / `try` / `function` at the bracket depth where
    it began. It moves from its subject (before `with`) to a pattern
    (before an arm's `->`) to an arm body (until the next `|`). The first
    `match` / `function` arm is the default path; every `try` handler
    arm is a decision.'''
    # pylint: disable=R0903

    def __init__(self, depth, phase, counts_first_arm):
        self.depth, self.phase = depth, phase
        self.counts_arm = counts_first_arm

    def arm_token(self, token):
        '''Advance on `|` or `->`; return whether it adds a decision.'''
        if self.phase == 'subject':
            return False
        if token == '|':
            self.phase = 'pattern'
            return False
        if self.phase != 'pattern':
            return False
        counts, self.counts_arm = self.counts_arm, True
        self.phase = 'body'
        return counts


class ArmTracker:
    '''The open arm constructs of one item, innermost last.'''

    def __init__(self):
        self._scopes = []

    def adds_decision(self, token, depth):
        '''Follow `token` at bracket `depth`; return whether it adds
        a decision.'''
        if token in ('match', 'try'):
            self._scopes.append(_ArmScope(depth, 'subject', token == 'try'))
        elif token == 'function':
            self._scopes.append(_ArmScope(depth, 'pattern', False))
        elif token == 'with':
            self._start_arms(depth)
        elif token in ('->', '|') and self._scopes:
            scope = self._scopes[-1]
            return scope.depth == depth and scope.arm_token(token)
        return False

    def in_pattern(self, depth):
        '''Whether the innermost construct is in an arm's pattern at
        `depth`, where `when` starts a guard.'''
        return bool(self._scopes) and self._scopes[-1].depth == depth and \
            self._scopes[-1].phase == 'pattern'

    def close_to(self, depth):
        '''A closer ends the constructs opened inside it.'''
        while self._scopes and self._scopes[-1].depth > depth:
            self._scopes.pop()

    def _start_arms(self, depth):
        '''`with` starts the arms of the innermost `match` / `try` still
        in its subject at this depth, ending constructs opened since; a
        record update's `with` is inside braces and finds none.'''
        for index in range(len(self._scopes) - 1, -1, -1):
            scope = self._scopes[index]
            if scope.depth != depth:
                return
            if scope.phase == 'subject':
                del self._scopes[index + 1:]
                scope.phase = 'pattern'
                return
