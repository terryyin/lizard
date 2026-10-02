''' Language parser for Python '''

from .code_reader import CodeReader
from .script_language import ScriptLanguageMixIn
from .python_structure import PythonIndents, PythonStates, count_spaces
from .python_tokens import generate_tokens as generate_python_tokens


class PythonReader(CodeReader, ScriptLanguageMixIn):

    ext = ['py']
    language_names = ['python']

    # Separated condition categories
    _control_flow_keywords = {'if', 'elif', 'for', 'while', 'except', 'finally'}
    _logical_operators = {'and', 'or'}
    _case_keywords = set()  # Python uses if/elif, not case
    _ternary_operators = set()  # Python uses 'x if c else y' syntax, not ?

    # Tokens that, when immediately following 'case' or 'match' at line start,
    # indicate a variable rather than a soft keyword.
    # '.' → attribute access (case.value, match.group)
    # '=' → assignment (case = 5)
    # ':' → annotated assignment (case: int = 5)
    # ',' → tuple unpacking (case, other = 1, 2)
    # compound assignments (case += 1, etc.)
    # '(' and '[' require deeper lookahead (see _soft_keyword_lookahead) to
    # distinguish pattern starters ('case (x, y):') from function calls
    # ('case(x)') and subscripts ('case[0]').
    _SOFT_KW_VARIABLE_NEXT = frozenset((
        '=', '.', ':', ',',
        '+=', '-=', '*=', '/=', '%=',
        '//=', '**=', '&=', '|=', '^=', '<<=', '>>=', ':=',
    ))

    # Tokens preceding a triple-quoted operand rather than a standalone string.
    _EXPRESSION_PRECEDING_TOKENS = frozenset((
        '=', '+=', '-=', '*=', '/=', '%=', '//=', '**=', '&=', '|=', '^=',
        '<<=', '>>=', '(', 'return', ',', '[', '+', '-', '*', '/', '//', '%',
    ))

    def __init__(self, context):
        super(PythonReader, self).__init__(context)
        self.parallel_states = [PythonStates(context, self)]
        self._last_meaningful_token = None  # Track the last meaningful token
        self._keyword_case = False   # set by _soft_keyword_lookahead: True when 'case' is a soft keyword
        self._keyword_match = False  # set by _soft_keyword_lookahead: True when 'match' is a soft keyword
        self.one_line_function = None  # set by PythonStates: body on the line of the def

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return generate_python_tokens(source_code, addition, token_class)

    def process_token(self, token):
        """Process triple-quoted strings used as comments, and Python soft keywords.

        Triple-quoted strings that are not docstrings (i.e., not immediately
        after function definitions) should be treated like comments and not
        counted in NLOC, but only if they appear to be standalone statements
        rather than part of assignments or other expressions.

        'case' used as a soft keyword adds +1 to cyclomatic complexity.
        The keyword-vs-variable distinction is made in _soft_keyword_lookahead
        (called from preprocess) and stored in self._keyword_case before this
        method is invoked.  'match' itself adds no complexity for regular CCN;
        the --modified extension adds +1 for the block via self._keyword_match.

        Returns:
            bool: True if the token was handled specially, False otherwise
        """
        # --- Soft keyword: case ---
        # _keyword_case is pre-set by _soft_keyword_lookahead in preprocess().
        if token == 'case' and self._keyword_case:
            self.context.add_condition()

        # --- Triple-quoted string comment suppression ---
        if (token.startswith('"""') or token.startswith("'''")) and len(token) >= 6:
            # Check if this is likely a standalone comment (not a docstring)
            # Docstrings are handled separately in _state_first_line
            current_state = self.parallel_states[0]._state

            # If we're not in the first line state, check if this is a standalone string
            if current_state != current_state.__self__._state_first_line:
                # Check if the immediate previous meaningful token suggests this is part of an expression
                is_part_of_expression = self._last_meaningful_token in self._EXPRESSION_PRECEDING_TOKENS

                # Only treat as comment if it's NOT part of an expression
                if not is_part_of_expression:
                    # Subtract the NLOC contribution of this triple-quoted string
                    self.context.add_nloc(-(token.count('\n') + 1))

        # Update last meaningful token (ignore whitespace and newlines)
        if token not in ['\n', ' ', '\t'] and not token.isspace():
            self._last_meaningful_token = token

        return False  # Continue with normal processing

    def _soft_keyword_lookahead(self, tokens):
        """Wrap the token stream to pre-detect Python soft keywords.

        Yields tokens in the original order.  Before yielding 'case' or
        'match' at the start of a statement, reads ahead to the next
        non-whitespace token and sets self._keyword_case / self._keyword_match
        so that downstream consumers (extensions and process_token) can read
        the flag when they receive that token.

        Detection rules (applied in order):
        - If the next non-whitespace token is in _SOFT_KW_VARIABLE_NEXT
          ('=', '.', ':', ',', compound assignments) → variable.
        - If the next non-whitespace token is '(' or '[', scan forward
          tracking bracket depth across all bracket types; if a ':' at
          depth 0 is found before end-of-statement → keyword (case pattern
          or match subject with optional guard), otherwise → variable
          (function call or subscript).
        - Any other following token → keyword.
        """
        at_line_start = True
        tokens_iter = iter(tokens)
        for token in tokens_iter:
            if token == '\n':
                at_line_start = True
                yield token
            elif token.isspace():
                yield token
            elif at_line_start and token in ('case', 'match'):
                # Buffer this token; peek at next non-whitespace.
                lookahead = []
                next_real = None
                for t in tokens_iter:
                    lookahead.append(t)
                    if t != '\n' and not t.isspace():
                        next_real = t
                        break
                if next_real in self._SOFT_KW_VARIABLE_NEXT:
                    is_keyword = False
                elif next_real in ('(', '['):
                    # '(' and '[' are valid pattern starters in match/case
                    # ('case (x, y):' / 'case [0]:') but also appear in
                    # function calls ('case(x)') and subscripts ('case[0]').
                    # Disambiguate by scanning past the balanced bracket group
                    # and checking whether a ':' at depth 0 follows — that
                    # colon is the one that opens the case suite.  Guards
                    # ('case (x, y) if cond:') are handled naturally because
                    # the scan continues until it finds the ':'.
                    is_keyword = False
                    depth = 1  # the opening bracket is already in lookahead
                    for t in tokens_iter:
                        lookahead.append(t)
                        if t in ('(', '[', '{'):
                            depth += 1
                        elif t in (')', ']', '}'):
                            depth -= 1
                        elif t == ':' and depth == 0:
                            is_keyword = True
                            break
                        elif t == '\n' and depth == 0:
                            break  # end of statement — no pattern colon found
                else:
                    is_keyword = True
                if token == 'case':
                    self._keyword_case = is_keyword
                else:
                    self._keyword_match = is_keyword
                at_line_start = False
                yield token
                for t in lookahead:
                    yield t
            else:
                # Not a soft keyword at line start — clear stale flags.
                if token == 'case':
                    self._keyword_case = False
                elif token == 'match':
                    self._keyword_match = False
                at_line_start = False
                yield token

    def preprocess(self, tokens):
        indents = PythonIndents(self.context)
        current_leading_spaces = 0
        reading_leading_space = True
        brackets = 0
        for token in self._soft_keyword_lookahead(tokens):
            if token != '\n':
                # A line that starts with the closing bracket of a bracket
                # open on a line before goes on with that line: its
                # indentation is not the one of a block. A definition cannot
                # be inside brackets: one left open is closed there.
                if reading_leading_space and token in ('def', 'class'):
                    brackets = 0
                continued = brackets > 0 and token in (')', ']', '}')
                if token in ('(', '[', '{'):
                    brackets += 1
                elif token in (')', ']', '}'):
                    brackets -= 1
                if reading_leading_space:
                    if token.isspace():
                        current_leading_spaces += count_spaces(token)
                    else:
                        if not token.startswith('#') and not continued:
                            current_function = self.context.current_function
                            if (current_function.name == '*global*' or
                                    current_function.long_name.endswith(')')):
                                indents.set_nesting(current_leading_spaces, token)
                        reading_leading_space = False
            else:
                if brackets <= 0:
                    brackets = 0
                    self._end_one_line_function()
                reading_leading_space = True
                current_leading_spaces = 0
            if not token.isspace() or token == '\n':
                yield token
        self._end_one_line_function()
        indents.reset()

    def _end_one_line_function(self):
        """End a function whose body is on the line of its def, such as
        'def f(a): return a'. With no indented block after it, the function
        would otherwise never end."""
        function, self.one_line_function = self.one_line_function, None
        if function is None or self.context.current_function is not function:
            return
        self.context.start_new_function_nesting(None)
        self.context.end_of_function()
        self.context.current_function = (
            self.context.last_function or self.context.global_pseudo_function)
