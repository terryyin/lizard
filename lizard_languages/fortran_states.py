'''
State machine for Fortran procedures and nested scopes.
'''

import re
from .code_reader import CodeStateMachine


FORTRAN_BLOCKS = [
    'PROGRAM', 'MODULE', 'SUBMODULE', 'SUBROUTINE', 'FUNCTION', 'TYPE',
    'INTERFACE', 'BLOCK', 'IF', 'DO', 'FORALL', 'WHERE', 'SELECT', 'ASSOCIATE'
]
FORTRAN_BLOCK_ENDINGS = '|'.join(r'END\s*{0}'.format(_) for _ in FORTRAN_BLOCKS)


class FortranStates(CodeStateMachine):
    _ends = re.compile(FORTRAN_BLOCK_ENDINGS, re.I)

    # Define token groups to eliminate duplication
    SAME_LINE_IGNORE_TOKENS = {'SAVE', 'DATA'}
    IGNORE_NEXT_TOKENS = {'%', '::'} | SAME_LINE_IGNORE_TOKENS
    IGNORE_VAR_TOKENS = {'INTEGER', 'REAL', 'COMPLEX', 'LOGICAL', 'CHARACTER'}
    RESET_STATE_TOKENS = {'RECURSIVE', 'ELEMENTAL'}
    FUNCTION_NAME_TOKENS = {'SUBROUTINE', 'FUNCTION'}
    NESTING_KEYWORDS = {'FORALL', 'SELECT', 'INTERFACE', 'ASSOCIATE'}
    PROCEDURE_TOKENS = {'PROCEDURE', 'MODULE PROCEDURE'}

    def __init__(self, context, reader):
        super().__init__(context)
        self.reader = reader
        self.last_token = None
        self.in_interface = False

    def __call__(self, token, reader=None):
        if self.reader.macro_disabled:
            return
        if self._state(token):
            self.next(self.saved_state)
            if self.callback:
                self.callback()
        self.last_token = token
        if self.to_exit:
            return True

    def _state_global(self, token):
        token_upper = token.upper()
        if token_upper in self.IGNORE_NEXT_TOKENS:
            self._state = self._ignore_next
        elif token_upper in self.IGNORE_VAR_TOKENS:
            self._state = self._ignore_var
        elif token == '(':
            self.next(self._ignore_expr, token)
        elif token_upper in self.RESET_STATE_TOKENS:
            self.reset_state()
        elif token_upper in self.FUNCTION_NAME_TOKENS:
            self._state = self._function_name
        elif token_upper == 'PROGRAM':
            self._state = self._namespace
        elif token_upper == 'MODULE':
            self._state = self._module_or_procedure
        elif token_upper == 'SUBMODULE':
            self._state = self._module
            self._module(token)
        elif token_upper == 'TYPE':
            self._state = self._type
        elif token_upper == 'IF':
            self._state = self._if
        elif token_upper == 'WHERE':
            self._state = self._where
        elif token_upper == 'BLOCK':
            self._state = self._ignore_if_paren
        elif token_upper == 'DO':
            self._state = self._ignore_if_label
        elif token_upper in self.NESTING_KEYWORDS:
            self.context.add_bare_nesting()
            if token_upper == 'INTERFACE':
                self.in_interface = True
        elif token_upper == 'ELSE':
            self.context.pop_nesting()
            self.context.add_bare_nesting()
        elif token_upper.replace(' ', '') == 'ELSEIF':
            self.context.pop_nesting()
            if token_upper == 'ELSEIF':
                self.context.add_condition()
            self._state = self._if
        elif token_upper == 'END' or self._ends.match(token):
            end_token_upper = token_upper.replace(' ', '')
            if end_token_upper.startswith('ENDINTERFACE'):
                self.in_interface = False
            self.context.pop_nesting()

    def reset_state(self, token=None):
        self._state = self._state_global
        if token is not None:
            self._state_global(token)

    def _ignore_next(self, token):
        if (self.last_token.upper() in self.SAME_LINE_IGNORE_TOKENS
                and self.context.newline):
            self.reset_state(token)
        else:
            self.reset_state()

    def _ignore_var(self, token):
        if token.upper() in self.FUNCTION_NAME_TOKENS:
            self.reset_state(token)
        else:
            self.reset_state()

    def _ignore_if_paren(self, token):
        if token == '(':
            self.next(self._ignore_expr, token)
        else:
            self.context.add_bare_nesting()
            self.reset_state()

    def _ignore_if_label(self, token):
        if token.isdigit():
            self.reset_state()
        else:
            self.context.add_bare_nesting()
            self.reset_state(token)

    @CodeStateMachine.read_inside_brackets_then('()', '_state_global')
    def _ignore_expr(self, token):
        pass

    def _function_name(self, token):
        self.context.restart_new_function(token)
        self.context.add_to_long_function_name('(')
        self._state = self._function_has_param

    def _function_has_param(self, token):
        if token == '(':
            self.next(self._function_params, token)
        else:
            self._function(token)

    @CodeStateMachine.read_inside_brackets_then('()', '_function')
    def _function_params(self, token):
        if token not in '()':
            self.context.parameter(token)

    def _function(self, token):
        self.context.add_to_long_function_name(' )')
        self.context.add_bare_nesting()
        self.reset_state(token)

    def _module(self, token):
        if token.upper() in self.FUNCTION_NAME_TOKENS:
            self._state = self._function_name
        elif token.upper() in self.PROCEDURE_TOKENS:
            self._state = self._procedure
        else:
            self._namespace(token)

    def _procedure(self, token):
        # Start a new function regardless of context
        if self.last_token and self.last_token.upper() == 'MODULE':
            # For "module procedure" case, use the current token as function name
            self.context.restart_new_function(token)
        else:
            # For standalone "procedure" case
            self.context.restart_new_function(token)
        self.context.add_bare_nesting()
        self.reset_state()

    def _type(self, token):
        if token.upper() == 'IS':
            self.type_name = token
            self._state = self._type_is
        elif token in (',', '::') or token[0].isalpha():
            self._namespace(token)
        else:
            self.reset_state(token)

    def _type_is(self, token):
        if token == '&':
            return
        if token != '(':
            self.context.add_namespace(self.type_name)
        self.reset_state(token)

    def _namespace(self, token):
        self.context.add_namespace(token)
        self.reset_state()

    def _if(self, token):
        if token == '(':
            self.next(self._if_cond, token)
        else:
            self.reset_state(token)

    @CodeStateMachine.read_inside_brackets_then('()', '_if_then')
    def _if_cond(self, token):
        pass

    def _if_then(self, token):
        token_upper = token.upper()
        if token_upper == 'THEN':
            self.context.add_bare_nesting()
            self.reset_state()
        else:
            self.reset_state(token)

    def _where(self, token):
        if token == '&':
            return
        if token == '(':
            self.next(self._where_mask, token)
        else:
            self.reset_state(token)

    @CodeStateMachine.read_inside_brackets_then('()', '_where_body')
    def _where_mask(self, token):
        self.where_mask_line = self.context.current_line
        self.where_continued = False

    def _where_body(self, token):
        if token == '&':
            self.where_continued = True
            return
        if token == ';' or (self.context.current_line > self.where_mask_line
                            and not self.where_continued):
            self.context.add_bare_nesting()
        self.reset_state(token)

    def _module_or_procedure(self, token):
        if token.upper() == 'PROCEDURE':
            self._state = self._procedure
        else:
            self._module(token)
