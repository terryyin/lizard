''' Language parser for TTCN-3 '''

import re
from lizard import CLikeReader, CodeReader


class TTCNReader(CLikeReader, CodeReader):  # pylint: disable=R0903

    ext = ['ttcn', 'ttcnpp']
    language_names = ['ttcn', 'ttcn3']

    conditions = set(['if', 'else', 'for', 'while', 'altstep',
                      'case', 'goto', 'alt', 'interleave',
                      'and', 'or', 'xor'])

    parameter_bracket_open = '('
    parameter_bracket_close = ')'

    def __init__(self, context):
        super(TTCNReader, self).__init__(context)

    # module and group blocks are ignored
    def _state_global(self, token):
        if token == 'testcase':
            self._state = self._state_function
            self.context.start_new_function('__testcase__')
        elif token == 'function':
            self._state = self._state_function
            self.context.start_new_function('')
        elif token == 'control':
            self.context.start_new_function('__control__')
            self._state = self._state_dec_to_imp

    def _state_function(self, token):
        if token[0].isalpha():
            self.context.add_to_function_name(token)
        elif token == '(':
            self.next(self._state_dec, token)
        elif token == '@deterministic':
            self.context.add_to_long_function_name(token + ' ')
        else:
            self._state = self._state_global

    def _state_dec_to_imp(self, token):
        if token == '{':
            self.next(self._state_imp, "{")
        else:
            self.context.add_to_long_function_name(' ' + token)

    @staticmethod
    def generate_tokens(source_code, _=None):
        return CodeReader.generate_tokens(
            source_code,
            r'|' + r'|'.join(re.escape(s) for s in (
                '..', '->', '<@', '@>', '@lazy', '@fuzzy',
                '@index', '@deterministic')))
