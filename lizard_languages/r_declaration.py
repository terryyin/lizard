"""R function declaration name recovery and parameter header consumption."""

_LEFT_ASSIGNMENT_OPS = ('<-', '=')
_NAME_STOP_TOKENS = ('function', '\\', '(', ')', '{', '}', '\n')
_MAX_RECENT_TOKENS = 10


def is_r_identifier_token(token):
    """Return True if token consists of valid R identifier characters or dots."""
    return token.replace('_', 'a').replace('.', 'a').isalnum() or token == '.'


def has_left_assignment(recent_tokens):
    """Return True when recent tokens end with `<assignment_op>, 'function'`."""
    return len(recent_tokens) >= 2 and recent_tokens[-2] in _LEFT_ASSIGNMENT_OPS


def extract_left_assignment_names(recent_tokens):
    """Recover left-assigned function names from recent tokens, left to right."""
    if len(recent_tokens) < 3:
        return ["(anonymous)"]

    # For multiple assignment like: a <- b <- c <- function(...)
    # recent_tokens ends with [..., 'a', '<-', 'b', '<-', 'c', '<-', 'function']
    assignment_index = len(recent_tokens) - 2
    function_names = []
    i = assignment_index - 1
    current_name_tokens = []

    while i >= 0:
        token = recent_tokens[i]

        if token in _LEFT_ASSIGNMENT_OPS:
            if current_name_tokens:
                function_names.append(''.join(reversed(current_name_tokens)))
                current_name_tokens = []
            i -= 1
            continue

        if token in _NAME_STOP_TOKENS:
            break

        if is_r_identifier_token(token):
            current_name_tokens.append(token)
            i -= 1
        else:
            break

    if current_name_tokens:
        function_names.append(''.join(reversed(current_name_tokens)))

    return list(reversed(function_names)) if function_names else ["(anonymous)"]


def extract_formal_name(tokens):
    """Return the authoritative R formal parameter name for one declaration."""
    head = []
    for token in tokens:
        if token == '=':
            break
        head.append(token)
    if not head:
        return ''
    if len(head) == 1 and len(head[0]) >= 2 and head[0][0] == '`' and head[0][-1] == '`':
        return head[0][1:-1]
    return ''.join(head)


class RDeclarationMixin:
    """Mixin handling R function declarations from `function` through body handoff."""

    def _record_recent_token(self, token):
        """Record non-whitespace tokens for left-assignment name recovery."""
        if not token.isspace() and token != '\n':
            self.recent_tokens.append(token)
            if len(self.recent_tokens) > _MAX_RECENT_TOKENS:
                self.recent_tokens.pop(0)

    def _start_declaration(self, allow_anonymous=True, before_start=None):
        """Start a function declaration when `function` is encountered."""
        if has_left_assignment(self.recent_tokens):
            func_names = extract_left_assignment_names(self.recent_tokens)
        elif allow_anonymous:
            func_names = ["(anonymous)"]
        else:
            return False

        if before_start is not None:
            before_start()
        self._begin_declaration(func_names)
        return True

    def _begin_declaration(self, func_names):
        """Open the primary function, queue chained names, and consume params."""
        self.context.restart_new_function(func_names[0])
        self.additional_function_names = func_names[1:]
        self._state = self._function_params

    def _function_params(self, token):
        """Expecting function parameters."""
        if token == '(':
            self.context.add_to_long_function_name("(")
            self._parameter_depth = 0
            self._parameter_tokens = []
            self._state = self._read_params
        else:
            # Single expression function without parentheses - rare in R
            self._state = self._function_body
            self._function_body(token)

    def _read_params(self, token):
        """Read function parameters until closing parenthesis."""
        if token.isspace() or token.startswith('#'):
            return
        if token == ')' and self._parameter_depth == 0:
            self._finish_parameter()
            self.context.add_to_long_function_name(")")
            self._state = self._function_body
        elif token == ',' and self._parameter_depth == 0:
            self._finish_parameter()
        else:
            self._parameter_tokens.append(token)
            if token in ('(', '[', '{'):
                self._parameter_depth += 1
            elif token in (')', ']', '}'):
                self._parameter_depth = max(0, self._parameter_depth - 1)

    def _finish_parameter(self):
        """Commit one completed R formal declaration and its parameter name."""
        if self._parameter_tokens:
            declaration = ' '.join(self._parameter_tokens)
            name = extract_formal_name(self._parameter_tokens)
            self.context.parameter(declaration, name=name)
            self._parameter_tokens = []
