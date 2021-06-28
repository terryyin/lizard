'''
Language parser for Fortran.
'''

import re
from .code_reader import CodeStateMachine, CodeReader


# pylint: disable=R0903
class FortranCommentsMixin(object):
    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('!'):
            return token[1:]


# pylint: disable=R0903
class FortranReader(CodeReader, FortranCommentsMixin):
    ''' This is the reader for Fortran. '''

    ext = ['f70', 'f90', 'f95', 'f03', 'f08', 'f', 'for', 'ftn', 'fpp']
    language_names = ['fortran']
    _conditions = set((
        'IF', 'DO', '.AND.', '.OR.', 'CASE',
        'if', 'do', '.and.', '.or.', 'case'))
    _blocks = ['PROGRAM', 'MODULE', 'SUBROUTINE', 'FUNCTION', 'TYPE', 'INTERFACE', 'BLOCK', 'IF', 'DO', 'FORALL', 'WHERE', 'SELECT', 'ASSOCIATE']

    def __init__(self, context):
        super(FortranReader, self).__init__(context)
        self.macro_disabled = False
        self.parallel_states = [FortranStates(context, self)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        _until_end = r'(?:\\\n|[^\n])*'
        return CodeReader.generate_tokens(
            source_code,
            r'(?i)' +
            r'|\/\/' +
            r'|\#' + _until_end +
            r'|\!' + _until_end +
            r'|^C' + _until_end +
            r'|^\*' + _until_end +
            r'|\.OR\.' +
            r'|\.AND\.' +
            r'|ELSE +IF' +
            ''.join(r'|END[ \t]+{0}'.format(_) for _ in FortranReader._blocks) +
            addition,
            token_class)

    def preprocess(self, tokens):
        macro_depth = 0
        new_line = True
        for token in tokens:
            if new_line and token[0].upper() in ('c', 'C', '*'):
                token = '!'+token[1:]
            new_line = token == '\n'
            macro = re.match(r'#\s*(\w+)', token)
            if macro:
                macro = macro.group(1).lower()
                if macro in ('if', 'ifdef', 'ifndef', 'elif'):
                    self.context.add_condition()
                if macro_depth > 0:
                    if macro in ('if', 'ifdef', 'ifndef'):
                        macro_depth += 1
                    elif macro == 'endif':
                        macro_depth -= 1
                elif macro in ('else', 'elif'):
                    macro_depth += 1
                # In order to don't mess the nesting,
                # only the first branch of #if #elif #else
                # is read by the FortranStateMachine
                self.macro_disabled = macro_depth != 0
            elif not token.isspace() or token == '\n':
                yield token

# pylint: disable=R0903
class FortranStates(CodeStateMachine):
    # pylint: disable=line-too-long
    # pylint: disable=protected-access
    _ends = re.compile('(?:'+'|'.join(r'END\s*{0}'.format(_) for _ in FortranReader._blocks)+')', re.I)

    def __init__(self, context, reader):
        super(FortranStates, self).__init__(context)
        self.reader = reader

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
        if token_upper in ('%', '::', 'SAVE', 'DATA'):
            self._state = self._ignore_next
        elif token_upper in ('INTEGER', 'REAL','COMPLEX','LOGICAL', 'CHARACTER'):
            self._state = self._ignore_var
        elif token == '(':
            self.next(self._ignore_expr, token)
        elif token_upper in ('PROGRAM',):
            self._state = self._namespace
        elif token_upper == 'MODULE':
            self._state = self._module
        elif token_upper in ('SUBROUTINE', 'FUNCTION'):
            self._state = self._function_name
        elif token_upper == 'TYPE':
            self._state = self._type
        elif token_upper == 'IF':
            self._state = self._if
        elif token_upper in ('BLOCK',):
            self._state = self._ignore_if_paren
        elif token_upper in ('DO',):
            self._state = self._ignore_if_label
        elif token_upper in ('FORALL', 'WHERE', 'SELECT', 'INTERFACE', 'ASSOCIATE'):
            self.context.add_bare_nesting()
        elif token_upper == 'ELSE':
            self.context.pop_nesting()
            self.context.add_bare_nesting()
        elif token_upper.replace(' ', '') == 'ELSEIF':
            self.context.pop_nesting()
            self._state = self._if
        elif token_upper == 'END' or self._ends.match(token):
            self.context.pop_nesting()

    def reset_state(self, token=None):
        self._state = self._state_global
        if token is not None:
            self._state_global(token)

    # pylint: disable=unused-argument
    def _ignore_next(self, token):
        self.reset_state()

    def _ignore_var(self, token):
        if token.upper() in ('SUBROUTINE', 'FUNCTION'):
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
        if all(char in "0123456789" for char in token):
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
        if token.upper() == 'PROCEDURE':
            self.reset_state()
        else:
            self._namespace(token)

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
        if token.upper() == 'THEN':
            self.context.add_bare_nesting()
            self.reset_state()
        else:
            self.reset_state(token)
