"""
Base class for all language parsers
"""

import re
from copy import copy
from functools import reduce
from operator import or_


class CodeStateMachine:
    """ the state machine """
    # pylint: disable=R0903
    # pylint: disable=R0902
    # pylint: disable=E1102
    # disabled E1102 see https://github.com/PyCQA/pylint/issues/1493
    def __init__(self, context):
        self.context = context
        self.saved_state = self._state = self._state_global
        self.last_token = None
        self.to_exit = False
        self.callback = None
        self.rut_tokens = []
        self.br_count = 0

    def statemachine_clone(self):
        return self.__class__(self.context)

    def next(self, state, token=None):
        self._state = state
        if token is not None:
            return self(token)

    def next_if(self, state, token, expected):
        if token != expected:
            return
        self.next(state, token)

    def statemachine_return(self):
        self.to_exit = True
        self.statemachine_before_return()

    def sub_state(self, state, callback=None, token=None):
        self.saved_state = self._state
        self.callback = callback
        self.next(state, token)

    def __call__(self, token, reader=None):
        if self._state(token):
            self.next(self.saved_state)
            if self.callback:
                self.callback()
                self.callback = None
        self.last_token = token
        if self.to_exit:
            return True

    def _state_global(self, token):
        pass

    def statemachine_before_return(self):
        pass

    @staticmethod
    def read_inside_brackets_then(brs, end_state=None):
        def decorator(func):
            def read_until_matching_brackets(self, token):
                self.br_count += {brs[0]: 1, brs[1]: -1}.get(token, 0)
                if self.br_count == 0 or end_state is not None:
                    func(self, token)
                if self.br_count == 0 and end_state is not None:
                    self.next(getattr(self, end_state))
            return read_until_matching_brackets
        return decorator

    @staticmethod
    def read_until_then(tokens):
        def decorator(func):
            def read_until_then_token(self, token):
                if token in tokens:
                    func(self, token, self.rut_tokens)
                    self.rut_tokens = []
                else:
                    self.rut_tokens.append(token)
            return read_until_then_token
        return decorator


class CodeReader:
    """ CodeReaders are used to parse function structures from
    code of different
    language. Each language will need a subclass of CodeReader.  """

    ext = []
    languages = None
    extra_subclasses = set()
    _conditions = {'if', 'for', 'while', '&&', '||', '?', 'catch', 'case'}

    def __init__(self, context):
        self.parallel_states = []
        self.context = context
        self.conditions = copy(self._conditions)

    @classmethod
    def match_filename(cls, filename):
        def compile_file_extension_re(*exts):
            return re.compile(r".*\.(" + r"|".join(exts) + r")$", re.I)

        return compile_file_extension_re(*cls.ext).match(filename)

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        def create_token(match):
            return match.group(0)
        if not token_class:
            token_class = create_token

        def _generate_tokens(source, add, flags=0):
            # DO NOT put any sub groups in the regex. Good for performance
            _until_end = r"(?:\\\n|[^\n])*"
            combined_symbols = [
                "<<=", ">>=", "||", "&&", "===", "!==",
                "==", "!=", "<=", ">=", "->", "=>",
                "++", "--", '+=', '-=',
                "+", "-", '*', '/',
                '*=', '/=', '^=', '&=', '|=', "..."
            ]
            token_pattern = re.compile(
                r"(?:" +
                r"\/\*.*?\*\/" +
                add +
                r"|(?:\d+\')+\d+" +
                r"|0x(?:[0-9A-Fa-f]+\')+[0-9A-Fa-f]+" +
                r"|0b(?:[01]+\')+[01]+" +
                r"|\w+" +
                r"|\"(?:\\.|[^\"\\])*\"" +
                r"|\'(?:\\.|[^\'\\])*?\'" +
                r"|\/\/" + _until_end +
                r"|\#" +
                r"|:=|::|\*\*" +
                r"|\<(?=(?:[^<>]*\?)+[^<>]*\>)(?:[\w\s,.?]|(?:extends))+\>" +
                r"|" + r"|".join(re.escape(s) for s in combined_symbols) +
                r"|\\\n" +
                r"|\n" +
                r"|[^\S\n]+" +
                r"|.)", re.M | re.S | flags)
            macro = ""
            for match in token_pattern.finditer(source):
                token = token_class(match)
                if macro:
                    if "\\\n" in token or "\n" not in token:
                        macro += token
                    else:
                        yield macro
                        yield token
                        macro = ""
                elif token == "#":
                    macro = token
                else:
                    yield token
            if macro:
                yield macro

        flag_dict = {
            'a': re.A,  # ASCII-only matching
            'i': re.I,  # Ignore case
            'L': re.L,  # Locale dependent
            'm': re.M,  # Multi-line
            's': re.S,  # Dot matches all
            'u': re.U,  # Unicode matching
            'x': re.X   # Verbose
        }

        pattern = re.compile(r'\(\?[aiLmsux]+\)')
        re_flags = ''.join(opt[2:-1] for opt in pattern.findall(addition))
        flags = reduce(or_, (flag_dict[flag] for flag in re_flags), 0)
        cleaned_addition = pattern.sub('', addition)

        return _generate_tokens(
            source_code,
            cleaned_addition,
            flags=flags)

    def __call__(self, tokens, reader):
        self.context = reader.context
        for token in tokens:
            # Allow language-specific token processing
            if self.process_token(token):
                for state in self.parallel_states:
                    state(token)
                yield token
                continue

            for state in self.parallel_states:
                state(token)
            yield token
        for state in self.parallel_states:
            state.statemachine_before_return()
        self.eof()

    def eof(self):
        pass

    def process_token(self, token):
        """Process a token before normal handling.
        Return True if the token has been handled specially and should skip normal processing.

        Args:
            token: The token being processed

        Returns:
            bool: True if the token should skip normal processing, False otherwise
        """
        return False
