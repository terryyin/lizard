'''
Language parser for C, C++ -like languages.
'''

import re
import itertools
from .code_reader import CodeStateMachine, CodeReader
from .clike_states import CLikeStates

# Tokenizer fragment for C++ empty-delimiter raw strings R"(...)" (optional u8/u/U/L
# prefix). Stops at the first ')"'. Custom delimiters use the normal string rule.
# See #451, #478; subgroup-free per CodeReader.generate_tokens.
_CPP_RAW_STRING_TOKEN = r"|(?:u8|u|U|L)?R\"\((?:[^)]|\)(?!\"))*\)\""
# In C and C++, a backslash at the end of a line continues a // comment on the next line.
_C_LINE_COMMENT = r"|\/\/(?:\\\n|[^\n])*"


class CCppCommentsMixin(object):  # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("/*") or token.startswith("//"):
            return token[2:]


def clike_tokens(source_code, addition='', token_class=None):
    """The tokens of a C-like language, in which a // comment ends at its line."""
    addition = _CPP_RAW_STRING_TOKEN + \
        r"|(?:\d*\.\d+(?:[eE][-+]?\d+)?)" + \
        r"|(?:\d+\.(?:\d+)?(?:[eE][-+]?\d+)?)" + \
        addition
    return CodeReader.generate_tokens(source_code, addition, token_class)


# pylint: disable=R0903
class CLikeReader(CodeReader, CCppCommentsMixin):
    ''' This is the reader for C, C++ and Java. '''

    ext = ["c", "cpp", "cc", "cxx", "h", "hh", "hpp"]
    language_names = ['cpp', 'c']
    macro_pattern = re.compile(r"#\s*(\w+)\s*(.*)", re.M | re.S)

    def __init__(self, context):
        super(CLikeReader, self).__init__(context)
        self.parallel_states = (
                CLikeStates(context),
                CLikeNestingStackStates(context),
                CppRValueRefStates(context))

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return clike_tokens(source_code, addition + _C_LINE_COMMENT, token_class)

    def preprocess(self, tokens):
        tilde = False
        for token in tokens:
            if token == '~':
                tilde = True
            elif tilde:
                tilde = False
                yield "~" + token
            elif not token.isspace() or token == '\n':
                macro = self.macro_pattern.match(token)
                if macro:
                    if macro.group(1) in ('if', 'ifdef', 'elif'):
                        self.context.add_condition()
                    elif macro.group(1) == 'include':
                        yield "#include"
                        yield macro.group(2) or "\"\""
                    for _ in macro.group(2).split('\n')[1:]:
                        yield '\n'
                else:
                    yield token


class CppRValueRefStates(CodeStateMachine):
    # pylint: disable=R0903

    _UNARY_EXPRESSION_KEYWORDS = {
        "sizeof", "alignof", "_Alignof", "__alignof", "__alignof__",
        "__extension__", "delete", "not", "throw", "co_await", "co_yield",
    }

    def __init__(self, context):
        super().__init__(context)
        # None keeps the old count for a body with an ambiguous &&.
        self.restored_conditions = {}
        self.scan_function = None
        self.parenthesized_operand = False
        self.reference_attribute = False
        self.simple_prefix = True
        self.ambiguous_chain = False

    def _state_global(self, token):
        if token == "&&":
            function = self.context.current_function
            # The function body's opening brace resets the counts from its signature.
            self.scan_function = (function if self.context.current_nesting_level > function.top_nesting_level
                                  else None)
            self.parenthesized_operand = False
            self.reference_attribute = False
            self.simple_prefix = True
            self.ambiguous_chain = False
            self._state = self._r_value_ref
        elif token == "typedef":
            self.next(self._typedef)

    def _r_value_ref(self, token):
        if token in "=;{})":
            self._finish_reference_scan(token)
            self._state = self._state_global
        elif token == "(":
            self.parenthesized_operand = (self.last_token == "&&" or
                                          (self.last_token == "(" and self.parenthesized_operand))
        elif token == "&&":
            # A later && can only continue a simple chain in the initial operand.
            if not (self.parenthesized_operand and self.simple_prefix and self.last_token.isidentifier() and
                    self.last_token not in self._UNARY_EXPRESSION_KEYWORDS):
                self.ambiguous_chain = True
        elif token == "[" and self.last_token == "[":
            # A parenthesized reference declarator may contain an attribute.
            self.reference_attribute = True
        elif self.simple_prefix and not token.isidentifier():
            self.simple_prefix = False

    def _finish_reference_scan(self, token):
        function = self.context.current_function
        restored = None
        if function is self.scan_function:
            restored = self.restored_conditions.get(function, 0)
            if restored is not None and (token != "=" or self.ambiguous_chain):
                # An ambiguous && can already account for a decision restored in this body.
                if restored:
                    self.context.add_condition(-restored)
                self.restored_conditions[function] = restored = None
        if token == "=":
            if (restored is not None and self.parenthesized_operand and
                    not self.reference_attribute and self.last_token != "operator"):
                self.restored_conditions[function] = restored + 1
            else:
                self.context.add_condition(-1)

    @CodeStateMachine.read_until_then(';')
    def _typedef(self, _, tokens):
        self.context.add_condition(-tokens.count("&&"))
        self.next(self._state_global)


# pylint: disable=R0903
class CLikeNestingStackStates(CodeStateMachine):
    """Machinery to track nesting levels of tokens.

    The main indicators of nesting are braces: '{' and '}'.
    However, the code is complicated due to braceless structures,
    which are control-flow structures in C-like languages.
    Moreover, using braces as nesting level indicators leads to a big caveat;
    C++ initializer lists, uniform initialization,
    and any other constructs with braces add extra nesting level.

    Another complication comes from nested classes inside function bodies
    and control-flow declarations/bodies.
    The handling of these complex cases is unspecified and can be ignored.
    """

    __namespace_separators = ['<', ":", "final", "[", "extends", 'implements']
    _TYPE_KEYWORDS = ("struct", "class", "namespace", "union")

    def _state_global(self, token):
        """Dual-purpose state for global and structure bodies."""
        if token == "template":
            self._state = self._template_declaration
        elif token == ".":
            self._state = self._dot
        elif token in self._TYPE_KEYWORDS:
            self._state = self._read_namespace
        elif token == "{":
            self.context.add_bare_nesting()
        elif token == '}':
            self.context.pop_nesting()

    def _dot(self, _):
        self._state = self._state_global

    def _read_namespace(self, token):
        """Processes declarations right after namespace/class keywords."""
        if token == "[":
            self._state = self._read_attribute
        else:
            self._state = self._read_namespace_name
        self._state(token)

    @CodeStateMachine.read_until_then(')({;')
    def _read_namespace_name(self, token, saved):
        """Processes namespace/class/struct names from declarations."""
        self._state = self._state_global
        if token == "{":
            self.context.add_namespace(''.join(itertools.takewhile(
                lambda x: x not in self.__namespace_separators, saved)))

    @CodeStateMachine.read_inside_brackets_then("<>", "_state_global")
    def _template_declaration(self, _):
        """Ignores template parameters."""
        pass

    @CodeStateMachine.read_inside_brackets_then("[]", "_read_namespace")
    def _read_attribute(self, _):
        """Ignores C++11 attributes inside [[ ]]."""
        pass
