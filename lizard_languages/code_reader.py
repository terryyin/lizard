'''
Base class for all langauge parsers
'''

import re


class CodeStateMachine(object):
    ''' the state machine '''
    def __init__(self, context):
        self.context = context
        self._state = self._state_global
        self.br_count = 0
        self.rut_tokens = []

    @staticmethod
    def read_inside_brackets_then(brs, end_state=None):
        def decorator(func):
            def read_until_matching_brackets(self, token):
                self.br_count += {brs[0]: 1, brs[1]: -1}.get(token, 0)
                if self.br_count == 0 and end_state is not None:
                    self.next(getattr(self, end_state))
                if self.br_count == 0 or end_state is not None:
                    func(self, token)
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

    def __call__(self, tokens, reader):
        self.context = reader.context
        for token in tokens:
            self._state(token)
            yield token
        self.eof()

    def _state_global(self, token):
        pass

    def eof(self):
        pass

    def next(self, state, token=None):
        self._state = state
        if token is not None:
            self._state(token)


class CodeReader(CodeStateMachine):
    ''' CodeReaders are used to parse function structures from code of different
    language. Each language will need a subclass of CodeReader.  '''

    ext = []
    languages = None
    extra_subclasses = set()

    def __init__(self, context):
        super(CodeReader, self).__init__(context)

    @classmethod
    def match_filename(cls, filename):
        def compile_file_extension_re(*exts):
            return re.compile(r".*\.(" + r"|".join(exts) + r")$", re.I)

        return compile_file_extension_re(*cls.ext).match(filename)

    def eof(self):
        pass

    @staticmethod
    def generate_tokens(source_code, addition=''):
        def _generate_tokens(source_code, addition):
            # DO NOT put any sub groups in the regex. Good for performance
            _until_end = r"(?:\\\n|[^\n])*"
            combined_symbols = ["||", "&&", "===", "!==", "==", "!=", "<=",
                                ">=",
                                "++", "--", '+=', '-=',
                                '*=', '/=', '^=', '&=', '|=', "..."]
            token_pattern = re.compile(
                r"(?:" +
                r"/\*.*?\*/" +
                addition +
                r"|\w+" +
                r"|\"(?:\\.|[^\"\\])*\"" +
                r"|\'(?:\\.|[^\'\\])*?\'" +
                r"|//" + _until_end +
                r"|\#" +
                r"|:=|::|\*\*" +
                r"|" + r"|".join(re.escape(s) for s in combined_symbols) +
                r"|\\\n" +
                r"|\n" +
                r"|[^\S\n]+" +
                r"|.)", re.M | re.S)
            macro = ""
            for token in token_pattern.findall(source_code):
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

        return [t for t in _generate_tokens(source_code, addition)]
