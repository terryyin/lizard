'''
Language parser for Fortran.
'''

import re
from .code_reader import CodeStateMachine, CodeReader


class FortranCommentsMixin:
    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('!'):
            return token[1:]


class FortranReader(CodeReader, FortranCommentsMixin):
    '''This is the reader for Fortran.'''

    ext = ['f70', 'f90', 'f95', 'f03', 'f08', 'f', 'for', 'ftn', 'fpp']
    language_names = ['fortran']

    # Conditions need to have all the cases because the matching is case-insensitive
    # and is not done here.
    _conditions = {
        'IF', 'DO', '.AND.', '.OR.', 'CASE',
        'if', 'do', '.and.', '.or.', 'case'
    }
    _blocks = [
        'PROGRAM', 'MODULE', 'SUBMODULE', 'SUBROUTINE', 'FUNCTION', 'TYPE',
        'INTERFACE', 'BLOCK', 'IF', 'DO', 'FORALL', 'WHERE', 'SELECT', 'ASSOCIATE'
    ]

    def __init__(self, context):
        super().__init__(context)
        self.macro_disabled = False
        self.parallel_states = [FortranStates(context, self)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        _until_end = r'(?:\\\n|[^\n])*'
        block_endings = '|'.join(r'END\s*{0}'.format(_) for _ in FortranReader._blocks)
        # Include all patterns and the (?i) flag in addition
        addition = (
            r'(?i)'
            r'\/\/|'
            r'\#' + _until_end + r'|'
            r'\!' + _until_end + r'|'
            r'^\*' + _until_end + r'|'
            r'\.OR\.|'
            r'\.AND\.|'
            r'ELSE\s+IF|'
            r'MODULE\s+PROCEDURE|'
            + block_endings + addition
        )
        return CodeReader.generate_tokens(
            source_code, addition=addition, token_class=token_class)

    def preprocess(self, tokens):
        macro_depth = 0
        new_line = True
        for token in tokens:
            if new_line and token[0].upper() in ('C', '*'):
                token = '!' + token[1:]
            new_line = token == '\n'
            macro_match = re.match(r'#\s*(\w+)', token)
            if macro_match:
                macro = macro_match.group(1).lower()
                if macro in ('if', 'ifdef', 'ifndef', 'elif'):
                    self.context.add_condition()
                if macro_depth > 0:
                    if macro in ('if', 'ifdef', 'ifndef'):
                        macro_depth += 1
                    elif macro == 'endif':
                        macro_depth -= 1
                elif macro in ('else', 'elif'):
                    macro_depth += 1
                # Only the first branch of #if #elif #else is read
                self.macro_disabled = macro_depth != 0
            elif not token.isspace() or token == '\n':
                yield token


class FortranStates(CodeStateMachine):
    _ends = re.compile(
        '|'.join(r'END\s*{0}'.format(_) for _ in FortranReader._blocks), re.I)

    # Define token groups to eliminate duplication
    IGNORE_NEXT_TOKENS = {'%', '::', 'SAVE', 'DATA'}
    IGNORE_VAR_TOKENS = {'INTEGER', 'REAL', 'COMPLEX', 'LOGICAL', 'CHARACTER'}
    RESET_STATE_TOKENS = {'RECURSIVE', 'ELEMENTAL'}
    FUNCTION_NAME_TOKENS = {'SUBROUTINE', 'FUNCTION'}
    NESTING_KEYWORDS = {'FORALL', 'WHERE', 'SELECT', 'INTERFACE', 'ASSOCIATE'}
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
        if token in (',', '::') or token[0].isalpha():
            self._namespace(token)
        else:
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

    def _module_or_procedure(self, token):
        if token.upper() == 'PROCEDURE':
            self._state = self._procedure
        else:
            self._module(token)
