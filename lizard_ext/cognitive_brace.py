"""Brace, parenthesis, and semicolon nesting for cognitive complexity."""

from .cognitive_actions import _BraceActions
from .cognitive_counter import (
    BODY, BODY_PENDING, BRACE_HEAD, HEAD, PAREN_HEAD, _Counter, _Frame, _IF_LIKE)
from .cognitive_profile import _EXPRESSION_BREAKERS, _LAMBDA_TRIGGERS, _is_word


class _BraceCounter(_BraceActions, _Counter):  # pylint: disable=R0904
    """Nesting from braces, parentheses and semicolons (C-like languages).

    A structure is a frame: its *head* (``if (...)``) is followed by a body
    that is either braced or a single statement.  A finished body waits one
    token before its frame is popped, so that ``else``, ``catch``,
    ``finally`` and the ``while`` of a ``do`` bind to the right structure.
    """

    def __init__(self, reader, profile, gated):
        super(_BraceCounter, self).__init__(reader, profile, gated)
        self.lambda_triggers = _LAMBDA_TRIGGERS[profile.lambda_style]
        self.actions = self._keyword_actions(profile)
        if not profile.nesting:
            self._process = self._process_flat

    def _process(self, token):  # pylint: disable=E0202
        state = self.state
        line = self.context.current_line
        if line != state.line:
            self._new_line(line)
        if state.pending:
            self._resolve_pending(token)
        if _is_word(token):
            self._arm_word(token)
        if state.pending_logical is not None:
            if self._resolve_pending_logical(token):
                return
        if state.pending_ternary:
            self._resolve_pending_ternary(token)
        frames = state.frames
        if frames and frames[-1].closing:
            if self._resolve_closing(token):
                return
        if token in self.logical_ops:
            if token == '&&' and self.profile.rvalue_refs:
                if state.lambda_scan != 'params':   # ``[](T&& t)``: a type
                    state.pending_logical = token
                    state.pending_logical_word = False
                return
            self._logical_operator(token)
        elif token in _EXPRESSION_BREAKERS:
            state.logical.pop(state.pdepth, None)
        if token in ('(', '['):
            self._open_bracket()
        elif token in (')', ']'):
            self._close_bracket()
            self._pop_expression_frames()
        if state.lambda_scan is not None or token in self.lambda_triggers:
            if self._scan_lambda(token):
                return
        if frames:
            top = frames[-1]
            if top.kind == 'ternary':
                top = self._top_structure()
            if top is not None and top.phase != BODY and \
                    self._in_head(top, token):
                return
        action = self.actions.get(token)
        if action is not None:
            action()

    # -- statement boundaries ---------------------------------------------------

    def _new_line(self, line):
        state = self.state
        state.line = line
        if not self.profile.newline_ends_statement or state.pdepth or \
                state.prev == ',':
            return
        if state.frames and not state.frames[-1].closing:
            top = state.frames[-1]
            if top.phase == BODY and not top.braced:
                self._statement_end()

    def _top_structure(self):
        for frame in reversed(self.state.frames):
            if frame.kind != 'ternary':
                return frame
        return None

    def _pop_expression_frames(self):
        """Ternary operators end with their enclosing brackets."""
        frames = self.state.frames
        while frames and frames[-1].kind == 'ternary' and \
                frames[-1].pdepth > self.state.pdepth:
            frames.pop()

    def _statement_end(self):
        """A ';' at bracket depth 0 finishes the braceless bodies around it."""
        state = self.state
        frames = state.frames
        while frames and frames[-1].kind == 'ternary':
            frames.pop()
        for frame in reversed(frames):
            if frame.braced or frame.depth != state.depth or \
                    frame.phase not in (BODY_PENDING, BODY):
                break
            frame.phase = BODY
            frame.closing = True

    def _block_closed(self):
        """A '}' has just brought the brace depth down to state.depth."""
        state = self.state
        frames = state.frames
        while frames and frames[-1].depth > state.depth:
            frame = frames[-1]
            if frame.braced and frame.depth == state.depth + 1 and \
                    frame.kind != 'lambda':
                frame.closing = True   # this '}' ends its body
                break
            frames.pop()               # lived inside the closed block
        if state.pdepth:
            return
        index = len(frames) - 1
        while index >= 0 and frames[index].closing:
            index -= 1
        while index >= 0:              # braceless bodies made of this block
            frame = frames[index]
            if frame.braced or frame.depth != state.depth or \
                    frame.phase != BODY:
                break
            frame.closing = True
            index -= 1

    def _resolve_closing(self, token):
        """Now that the token after a finished body is known, decide whether
        it continues that structure.  Returns True if the token is consumed."""
        state = self.state
        frames = state.frames
        bound = False
        if token == 'else':
            bound = self._bind(_IF_LIKE)
        elif token in self.profile.catches or token == 'finally':
            self._bind(('catch',))
            bound = True                # continues the enclosing try statement
        elif token == 'while' and self._bind(('do',)):
            state.do_tail = True
            bound = True
        if bound:
            for frame in frames:
                frame.closing = False
        else:
            while frames and frames[-1].closing:
                frames.pop()
        if bound and token == 'else':
            self._hybrid()
            self._push('else', BODY_PENDING)
            return True
        return False

    def _bind(self, kinds):
        """Pop the innermost finished frame of the given kinds, and the
        finished frames above it.  Returns False if there is none."""
        frames = self.state.frames
        for index in range(len(frames) - 1, -1, -1):
            if not frames[index].closing:
                return False
            if frames[index].kind in kinds:
                del frames[index:]
                return True
        return False

    def _in_switch_body(self):
        top = self._top_structure()
        return (top is not None and top.kind == 'switch' and
                top.phase == BODY and top.depth == self.state.depth)

    # -- structure heads and bodies ---------------------------------------------

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

    def _push(self, kind, phase):
        state = self.state
        frame = _Frame(kind, phase, depth=state.depth, pdepth=state.pdepth)
        state.frames.append(frame)
        return frame

    def _open_block(self):
        self.state.depth += 1

    def _close_block(self):
        self.state.depth = max(0, self.state.depth - 1)
        self._block_closed()

    def _semicolon(self):
        if not self.state.pdepth:
            self._statement_end()
