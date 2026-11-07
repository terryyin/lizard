'''
State machine for COBOL PROCEDURE DIVISION paragraphs.
'''

from .code_reader import CodeStateMachine


# Synthetic tokens emitted by CobolReader.preprocess (newlines never reach
# the state machine).
PARAGRAPH = '@COBOL_PARAGRAPH'
SECTION = '@COBOL_SECTION'

DIVISION_STARTERS = {
    'IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE', 'ID',
}

# Ends an EVALUATE WHEN fall-through label sequence (values / THRU stay in-arm).
_EVALUATE_STATEMENT_STARTERS = {
    'MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'COMPUTE',
    'PERFORM', 'IF', 'ELSE', 'EVALUATE', 'SEARCH', 'GO', 'GOBACK',
    'EXIT', 'STOP', 'CALL', 'READ', 'WRITE', 'REWRITE', 'DELETE',
    'START', 'OPEN', 'CLOSE', 'DISPLAY', 'ACCEPT', 'SET', 'STRING',
    'UNSTRING', 'INSPECT', 'INITIALIZE', 'CONTINUE', 'NEXT', 'RETURN',
    'RELEASE', 'ENTRY', 'ALTER', 'MERGE', 'SORT', 'USE',
}


class CobolStates(CodeStateMachine):
    '''Paragraph-as-Function mapping for PROCEDURE DIVISION.'''

    def __init__(self, context):
        super(CobolStates, self).__init__(context)
        self.program_id = None
        self.in_procedure = False
        self.in_function = False
        self.waiting_program_id = False
        self._current_division = None
        # EVALUATE fall-through: consecutive WHEN labels share one arm (CCN).
        self.in_evaluate = False
        self.after_evaluate_when = False

    def finish(self):
        self._end_function()

    def statemachine_before_return(self):
        self.finish()

    def _end_function(self):
        if self.in_function:
            self.context.end_of_function()
            self.in_function = False

    def _start_function(self, name):
        self._end_function()
        self.context.restart_new_function(name)
        self.in_function = True

    def _ensure_preamble_function(self):
        if not self.in_function and self.in_procedure:
            name = self.program_id if self.program_id else '(procedure)'
            self._start_function(name)

    def reset_state(self, token=None):
        self._state = self._state_global
        if token is not None:
            self._state_global(token)

    def _state_global(self, token):
        token_upper = token.upper() if token else token

        if self.waiting_program_id:
            if token != '.':
                self.program_id = token
                self.waiting_program_id = False
            return

        if token == PARAGRAPH:
            if self.in_procedure:
                self.next(self._paragraph_name)
            return

        if token == SECTION:
            if self.in_procedure:
                self._end_function()
            self.next(self._skip_section_name)
            return

        if token_upper == 'PROGRAM-ID':
            self.waiting_program_id = True
            return

        if token_upper in DIVISION_STARTERS:
            self._current_division = token_upper
            self.next(self._expect_division_keyword)
            return

        if self.in_procedure and token not in ('.',):
            self._ensure_preamble_function()

        self._track_evaluate_when(token_upper)

    def _clear_evaluate_tracking(self):
        self.in_evaluate = False
        self.after_evaluate_when = False

    def _track_evaluate_when(self, token_upper):
        """Collapse fall-through WHEN labels inside EVALUATE to one arm.

        Shared condition_counter already counts each WHEN; subtract extras so
        ``WHEN 2 WHEN 3`` (one arm) matches the story's CCN 3 example.
        SEARCH WHEN is unaffected (not inside EVALUATE). Value tokens between
        fall-through WHENs (e.g. ``2``) stay in-arm; statement verbs end it.
        """
        if not self.in_procedure:
            return
        if token_upper == 'EVALUATE':
            self.in_evaluate = True
            self.after_evaluate_when = False
            return
        if token_upper in ('END-EVALUATE', 'END EVALUATE'):
            self._clear_evaluate_tracking()
            return
        if not self.in_evaluate:
            return
        if token_upper == 'WHEN':
            if self.after_evaluate_when:
                self.context.add_condition(-1)
            self.after_evaluate_when = True
            return
        if token_upper == 'WHEN OTHER':
            self.after_evaluate_when = False
            return
        if token_upper in _EVALUATE_STATEMENT_STARTERS:
            self.after_evaluate_when = False

    def _paragraph_name(self, token):
        self._start_function(token)
        self.reset_state()

    def _skip_section_name(self, token):
        # Name already consumed; just return to global.
        self.reset_state()

    def _expect_division_keyword(self, token):
        if token.upper() == 'DIVISION':
            self.next(self._after_division_keyword)
            return
        # e.g. PROCEDURE used as something else — uncommon after Area-A filter.
        saved = self._current_division
        self._current_division = None
        self.reset_state()
        if self.in_procedure:
            self._start_function(saved)

    def _after_division_keyword(self, token):
        # Optional period terminator from unrewritten DIVISION lines.
        if token == '.':
            self._apply_division_change()
            self.reset_state()
            return
        self._apply_division_change()
        self.reset_state(token)

    def _apply_division_change(self):
        division = (self._current_division or '').upper()
        self._current_division = None
        if division == 'PROCEDURE':
            self._end_function()
            self.in_procedure = True
        else:
            if self.in_procedure:
                self._end_function()
            self.in_procedure = False
        self._clear_evaluate_tracking()
