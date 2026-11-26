'''
Language parser for C Sharp
'''

from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates, clike_tokens


# The reserved words of C#, and `record`, which this reader also reads as
# a type keyword. Only these need `@` to be a name.
_KEYWORDS = (
    'abstract', 'as', 'base', 'bool', 'break', 'byte', 'case', 'catch',
    'char', 'checked', 'class', 'const', 'continue', 'decimal', 'default',
    'delegate', 'do', 'double', 'else', 'enum', 'event', 'explicit',
    'extern', 'false', 'finally', 'fixed', 'float', 'for', 'foreach', 'goto',
    'if', 'implicit', 'in', 'int', 'interface', 'internal', 'is', 'lock',
    'long', 'namespace', 'new', 'null', 'object', 'operator', 'out',
    'override', 'params', 'private', 'protected', 'public', 'readonly',
    'record', 'ref', 'return', 'sbyte', 'sealed', 'short', 'sizeof',
    'stackalloc', 'static', 'string', 'struct', 'switch', 'this', 'throw',
    'true', 'try', 'typeof', 'uint', 'ulong', 'unchecked', 'unsafe',
    'ushort', 'using', 'virtual', 'void', 'volatile', 'while')
_VERBATIM_KEYWORD = r"|@(?:" + "|".join(_KEYWORDS) + r")\b"


class CSharpReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['cs']
    language_names = ['csharp']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?', '??'}  # C# has both ?: and ?? (null-coalescing)

    def __init__(self, context):
        super(CSharpReader, self).__init__(context)
        self.parallel_states = [
            CSharpStates(context),
            _CSharpNestingStackStates(context)
        ]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # A verbatim identifier, `@if`, is one name and never a keyword.
        # Verbatim strings double quotes; raw strings match their quote count.
        # // comments end on the same line (not C/C++ line continuation).
        return clike_tokens(
            source_code,
            r'|@"(?:""|[^"])*"'
            r'|(?P<csharp_quotes>"{3,})(?!").*?(?P=csharp_quotes)(?!")'
            r"|(?:\?\?)" + _VERBATIM_KEYWORD, token_class)


def _name(token):
    """The name in a verbatim identifier: `@if` names `if`."""
    if token[0] == '@' and token[1:2].isidentifier():
        return token[1:]
    return token


class _CSharpNestingStackStates(CLikeNestingStackStates):
    # C# allows "template" as an ordinary identifier.
    _reads_cpp_templates = False


class CSharpStates(CLikeStates):
    def __init__(self, context):
        super(CSharpStates, self).__init__(context)
        self.in_primary_constructor = False
        self.class_name = None
        # (name to restore, brace depth when the type was declared). The
        # type's own "{" is not counted; "{" seen in the global state is.
        self._enclosing_classes = []
        self._open_braces = 0

    def try_new_function(self, name):
        if not self.in_primary_constructor:
            super(CSharpStates, self).try_new_function(name)
            if self.class_name and self.context.current_function:
                self.context.current_function.name = f"{self.class_name}::{name}"

    def _state_dec_to_imp(self, token):
        """Override to handle C# expression-bodied members (=>)"""
        if token == '=>':
            # Expression-bodied member: confirm function and enter expression body
            self.context.confirm_new_function()
            self._state = self._state_expression_body
        else:
            super(CSharpStates, self)._state_dec_to_imp(token)

    def _state_expression_body(self, token):
        """Read the expression body until semicolon, processing tokens for complexity"""
        if token == ';':
            # End of expression-bodied method, finalize function and return to global state
            self.context.end_of_function()
            self._state = self._state_global

    def _state_global(self, token):
        if token == '{':
            self._open_braces += 1
        elif token == '}':
            entered_at = (
                self._enclosing_classes[-1][1]
                if self._enclosing_classes else None)
            if entered_at is not None and self._open_braces == entered_at:
                self.class_name = self._enclosing_classes.pop()[0]
            elif self._open_braces > 0:
                self._open_braces -= 1
        if token in ("class", "struct", "record"):
            self._enclosing_classes.append(
                (self.class_name, self._open_braces))
            self.class_name = None
            self._state = self._state_class_declaration
        else:
            super(CSharpStates, self)._state_global(_name(token))

    def _state_class_declaration(self, token):
        if token == '(':  # Primary constructor
            self.in_primary_constructor = True
            self._state = self._state_primary_constructor
        elif token == '{':
            self._state = self._state_global
        elif _name(token)[0].isalpha():
            if not self.class_name:  # Only set class name if not already set
                self.class_name = _name(token)

    @CLikeStates.read_inside_brackets_then("()", "_state_class_declaration")
    def _state_primary_constructor(self, _):
        """Skip primary constructor parameters without counting them as a function"""
        self.in_primary_constructor = False
