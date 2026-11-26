'''
Language parser for Perl
'''

from .code_reader import CodeReader
from .perl_states import PerlStates
from .script_language import ScriptLanguageMixIn
from .perl_tokens import perl_tokens


_DOCUMENTATION = (
    r"^=[A-Za-z]\w*\b.*?(?:^=cut\b[^\n]*|\Z)"
    r"|^[ \t]*__(?:DATA|END)__\b.*\Z"
)


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

    # Separated condition categories
    _control_flow_keywords = {'if', 'elsif', 'unless', 'while', 'until', 'for',
                              'foreach', 'when', 'given', 'default', 'do'}
    _logical_operators = {'&&', '||', 'and', 'or'}
    _case_keywords = set()
    _ternary_operators = {'?', ':'}  # Both parts of ternary operator
    # `$obj->and()` / `Package::and()` name a member, not a decision.
    _BEFORE_MEMBER_NAME = frozenset(('->', '::'))

    def __init__(self, context):
        super(PerlReader, self).__init__(context)
        self.parallel_states = [PerlStates(context)]

    def preprocess(self, tokens):
        comment = None
        merged = []
        for token in tokens:
            if comment is not None:
                if token == '\n':
                    merged.append(comment)
                    comment = None
                    merged.append(token)
                else:
                    comment += token
            elif token == '#':
                comment = token
            else:
                merged.append(token)
        if comment is not None:
            merged.append(comment)
        return self.mark_member_names(merged, self._BEFORE_MEMBER_NAME)

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
            return stripped  # Return the stripped comment for other case
        return None

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return perl_tokens(source_code, addition, token_class, _DOCUMENTATION)
