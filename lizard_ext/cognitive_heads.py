"""Control heads and their transition into bodies in brace languages."""

from .cognitive_counter import BODY, BODY_PENDING, BRACE_HEAD, HEAD, PAREN_HEAD


class _BraceHeads(object):
    """Advance a control structure from its header to its body."""

    def _in_head(self, top, token):
        """Advance the head of the innermost structure; True if consumed."""
        state = self.state
        if top.phase == BODY_PENDING:
            return self._start_body(top, token)
        if top.phase == HEAD:
            if token == '(':
                top.phase = PAREN_HEAD
                top.pdepth = state.pdepth - 1
            elif token == '{' and state.pdepth == top.pdepth:
                self._open_braced_body(top)
            elif not self.profile.paren_heads:
                top.phase = BRACE_HEAD
            return True
        if top.phase == PAREN_HEAD:
            if token == ')' and state.pdepth == top.pdepth:
                top.phase = BODY_PENDING
            return True
        # BRACE_HEAD: Go / Rust / Swift ``if a && b {``
        if token == '{' and state.pdepth == top.pdepth:
            self._open_braced_body(top)
        return True

    def _start_body(self, top, token):
        if token == '{':
            self._open_braced_body(top)
            return True
        if top.kind == 'else' and token in self.profile.ifs:
            top.kind = 'else if'        # hybrid: already counted, no penalty
            top.phase = HEAD
            top.pdepth = self.state.pdepth
            return True
        top.phase = BODY
        top.braced = False
        top.depth = self.state.depth
        return False

    def _open_braced_body(self, frame):
        self.state.depth += 1
        frame.phase = BODY
        frame.braced = True
        frame.depth = self.state.depth
