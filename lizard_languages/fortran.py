'''
Language parser for Fortran.
'''

import re
from .code_reader import CodeReader
from .fortran_states import FortranStates, FORTRAN_BLOCKS, FORTRAN_BLOCK_ENDINGS


class FortranCommentsMixin:
    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('!'):
            return token[1:]


class FortranReader(CodeReader, FortranCommentsMixin):
    '''This is the reader for Fortran.'''

    ext = ['f70', 'f90', 'f95', 'f03', 'f08', 'f', 'for', 'ftn', 'fpp']
    language_names = ['fortran']

    # Separated condition categories (case-insensitive language)
    _control_flow_keywords = {'IF', 'DO', 'if', 'do'}
    _logical_operators = {'.AND.', '.OR.', '.and.', '.or.'}
    _case_keywords = {'CASE', 'case'}
    _ternary_operators = set()
    _blocks = FORTRAN_BLOCKS

    def __init__(self, context):
        super().__init__(context)
        self.macro_disabled = False
        self.parallel_states = [FortranStates(context, self)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        _until_end = r'(?:\\\n|[^\n])*'
        # Include all patterns and the (?i) flag in addition
        addition = (
            r'(?i)|'
            r'\/\/|'
            r'\#' + _until_end + r'|'
            r'\!' + _until_end + r'|'
            r'^\*' + _until_end + r'|'
            r'\.OR\.|'
            r'\.AND\.|'
            r'ELSE\s+IF|'
            r'MODULE\s+PROCEDURE|'
            + FORTRAN_BLOCK_ENDINGS + addition
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
