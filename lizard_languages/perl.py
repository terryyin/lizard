'''
Language parser for Perl
'''

from .code_reader import CodeReader, CodeStateMachine
from .script_language import ScriptLanguageMixIn


class PerlCommentsMixin(object):
    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('#'):
            return token  # Return the entire comment including #
        return None


class PerlReader(CodeReader, ScriptLanguageMixIn):
    # pylint: disable=R0903

    ext = ['pl', 'pm']
    language_names = ['perl']
    _conditions = set(['if', 'elsif', 'unless', 'while', 'until', 'for', 'foreach', '&&', '||', '?', 'when'])

    def __init__(self, context):
        super(PerlReader, self).__init__(context)
        self.parallel_states = [PerlStates(context)]

    def preprocess(self, tokens):
        comment = None
        for token in tokens:
            if comment is not None:
                if token == '\n':
                    yield comment
                    comment = None
                    yield token
                else:
                    comment += token
            elif token == '#':
                comment = token
            else:
                yield token
        if comment is not None:
            yield comment

    def _state(self, token):
        current_state = self.parallel_states[-1]
        if token == '\n':
            return
        return current_state.state(token)

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('#'):
            # For forgiveness comments, return the entire comment
            stripped = token.lstrip('#').strip()
            if stripped.startswith('lizard forgives') or stripped.startswith('#lizard forgives'):
                return '#lizard forgives'  # Return standardized forgiveness comment
            return stripped  # Return the stripped comment for other cases
        return None

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return ScriptLanguageMixIn.generate_common_tokens(source_code, addition, token_class)


class PerlStates(CodeStateMachine):
    _conditions = set(['if', 'elsif', 'unless', 'while', 'until', 'for', 'foreach'])

    def __init__(self, context):
        super(PerlStates, self).__init__(context)
        self.function_name = ''
        self.brace_count = 0
        self._state = self._state_global

    def _state_global(self, token):
        if token == 'sub':
            self.function_name = ''
            self.next(self._state_function_dec)
        elif token == '{':
            self.brace_count += 1
        elif token == '}':
            self.brace_count -= 1

    def _state_function_dec(self, token):
        if token == '{':
            self.brace_count = 1
            if self.function_name:
                self.context.try_new_function(self.function_name)
                self.context.confirm_new_function()
            self.next(self._state_function_body)
        elif token == ';':
            self.next(self._state_global)
        elif not token.isspace():
            self.function_name = token

    def _state_function_body(self, token):
        if token == '{':
            self.brace_count += 1
        elif token == '}':
            self.brace_count -= 1
            if self.brace_count == 0:
                self.context.end_of_function()
                self.next(self._state_global) 