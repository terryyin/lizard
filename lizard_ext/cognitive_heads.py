"""Control heads and their transition into bodies in brace languages."""

from .cognitive_counter import (
    BODY, BODY_PENDING, BRACE_HEAD, HEAD, PAREN_HEAD, _ATTRIBUTE_END,
    _INTERPOLATION_END, _ROUTED_ATTRIBUTE_END, _is_markup,
    _markup_opens_jsx_attribute)


class _BraceHeads(object):
    """Advance a control structure from its header to its body."""

    def _in_head(self, top, token):
        """Advance the head of the innermost structure; True if consumed."""
        state = self.state
        if top.phase == BODY_PENDING:
            return self._start_body(top, token)
        if top.phase == HEAD:
            # In a ternary-capable brace header, '(' can group just an operand.
            if token == '(' and (self.profile.paren_heads or
                                 not self.profile.ternary):
                top.phase = PAREN_HEAD
                top.pdepth = state.pdepth - 1
                return True
            if self.profile.paren_heads:
                if token == '{' and state.pdepth == top.pdepth:
                    self._open_braced_body(top)
                return True
            top.phase = BRACE_HEAD
        if top.phase == PAREN_HEAD:
            if token == ')' and state.pdepth == top.pdepth:
                top.phase = BODY_PENDING
                return True
        elif (token == '{' and state.pdepth == top.pdepth
              and state.depth == top.depth):
            self._pop_ternaries()
            self._open_braced_body(top)
            return True
        return self._in_head_expression(top, token)

    def _in_head_expression(self, top, token):
        if not self.profile.ternary:
            return True
        if _is_markup(token):
            if _markup_opens_jsx_attribute(token):
                self._open_block()
        elif token in ('{', '${'):
            self._open_block()
        elif (token == '}' or token is _INTERPOLATION_END
              or token is _ATTRIBUTE_END or token is _ROUTED_ATTRIBUTE_END):
            if self.state.depth > top.depth:
                self._close_block()
        elif token in (',', ';'):
            self._pop_expression_frames(peer_separator=True, same_brace=True)
        return token != '?'

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
