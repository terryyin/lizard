'''
Language parser for C, C++ -like languages.
'''

import re
import itertools
from .code_reader import CodeStateMachine, CodeReader
from .clike_states import CLikeStates

# Raw strings end only at the opening delimiter, which has at most 16 d-chars.
_CPP_RAW_STRING_TOKEN = (
    r'|(?:u8|u|U|L)?R"(?P<cpp_raw_delimiter>[^\s()\\]{0,16})'
    r'\(.*?\)(?P=cpp_raw_delimiter)"')
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
                CppRValueRefStates(context, self.conditions))

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

    # Shared headers can use C identifiers such as bool and wchar_t.
    _REFERENCE_TYPE_KEYWORDS = {
        "auto", "char", "short", "int", "long", "signed", "unsigned", "float", "double", "const", "volatile",
    }
    _REFERENCE_END = {">", ")", ",", ";"}
    _IDLE_TOKENS = {"&&", "typedef", "(", "["}
    _REFERENCE_SCAN_TOKENS = {"=", ";", "{", "}", ")", "&&", "[", "("}
    _LAMBDA_BEFORE = {"=", "(", "return", "co_return", "?", "&&", "||", "!", "+", "{", ",", ":"}
    _LAMBDA_GROUP_TOKENS = set("[](){}<>;=")
    _LAMBDA_IDLE_TOKENS = _LAMBDA_GROUP_TOKENS | _IDLE_TOKENS
    _LAMBDA_SCAN_TOKENS = _LAMBDA_GROUP_TOKENS | _REFERENCE_SCAN_TOKENS
    _LABEL_ADDRESS_PREFIX = {"(", "==", "!=", "not_eq", "!", "not", "sizeof", "__extension__"}
    _PARAMETER_TYPE_KEYWORDS = _REFERENCE_TYPE_KEYWORDS | {"bool", "wchar_t", "char8_t", "char16_t", "char32_t"}
    _NON_LOGICAL_AND = "operator or label"
    _GROUPED_OPERAND = "grouped operand"
    _GROUPED_SUBSCRIPT = "grouped subscript"

    def __init__(self, context, conditions):
        super().__init__(context)
        self._conditions = conditions
        self._recognize_references = context is not None and not context.fileinfo.filename.lower().endswith(".c")
        # Track whether a counted && can still be deducted at an initializer.
        self._initializer_deduction = None
        self._type_before_paren = False
        self._lambda = None
        self._events = self._IDLE_TOKENS

    def __call__(self, token, reader=None):
        events = self._events
        if events is None or token in events:
            self._state(token)
        self.last_token = token

    def _set_state(self, state, events=None):
        self._state = state
        self._events = events

    def _state_global(self, token):
        if token == "&&":
            self._initializer_deduction = None
            self._read_reference(token)
        elif token == "typedef":
            self._set_state(self._typedef)
        elif token == "(" and self._recognize_references:
            self._type_before_paren = self.last_token in self._REFERENCE_TYPE_KEYWORDS
        elif (token == "[" and self.last_token in self._LAMBDA_BEFORE and
              self._recognize_references and self._lambda is None):
            self._lambda = _LambdaHeader()
            self._track_lambda(self._lambda)

    def _read_reference(self, token):
        self._set_state(self._r_value_ref, self._REFERENCE_SCAN_TOKENS)
        if not self._counts_and(token):
            return
        previous = self.last_token
        if self._initializer_deduction is None:
            self._initializer_deduction = True
        if (self._initializer_deduction is not False and
                (previous in self._LABEL_ADDRESS_PREFIX or (previous == "operator" and self._recognize_references))):
            self._initializer_deduction = self._NON_LOGICAL_AND
        # A stand-alone && can be a macro argument. An operator name is also separate.
        if not self._recognize_references or previous in ("operator", ","):
            return
        if previous == "(":
            if self._type_before_paren:
                self._set_state(self._read_unnamed_reference_end)
        elif previous in self._REFERENCE_TYPE_KEYWORDS:
            self.context.add_condition(-1)
            self._initializer_deduction = False
        else:
            self._set_state(self._read_reference_end)

    def _read_unnamed_reference_end(self, token):
        if token == ")":
            self.context.add_condition(-1)
        self._continue_reference(token)

    def _read_reference_end(self, token):
        if token == "...":
            return
        # The ellipsis before a closing parenthesis can belong to a logical fold.
        if token in self._REFERENCE_END and (self.last_token != "..." or token in (">", ",")):
            self.context.add_condition(-1)
            self._initializer_deduction = False
        elif self._lambda is not None and self._lambda.in_parameter and token.isidentifier():
            self._lambda.references += 1
        self._continue_reference(token)

    def _continue_reference(self, token):
        self._set_state(self._r_value_ref, self._REFERENCE_SCAN_TOKENS)
        if token in self._REFERENCE_SCAN_TOKENS:
            self._r_value_ref(token)

    def _r_value_ref(self, token):
        if token == "&&":
            self._read_reference(token)
            return
        if token in ("[", "("):
            if token == "(":
                if self.last_token == "&&" and self._initializer_deduction is True:
                    self._initializer_deduction = self._GROUPED_OPERAND
                elif (self._initializer_deduction == self._GROUPED_OPERAND and
                      self._recognize_references and self.last_token not in ("&&", "(")):
                    self._set_state(self._read_grouped_argument)
            elif self._initializer_deduction == self._GROUPED_OPERAND and self._recognize_references:
                self._initializer_deduction = self._GROUPED_SUBSCRIPT
            self._state_global(token)
            return
        if token == "=" and self._initializer_deduction:
            if self._initializer_deduction is not True and self._has_grouped_assignment():
                self._set_state(self._read_grouped_assignment_rhs)
                return
            self.context.add_condition(-1)
        self._set_state(self._state_global, self._IDLE_TOKENS)

    def _read_grouped_argument(self, token):
        # A default argument includes a parameter declaration before '='.
        if self.last_token == "(" and token.isidentifier() and token not in self._PARAMETER_TYPE_KEYWORDS:
            return
        if token != "=":
            self._initializer_deduction = True
        self._continue_reference(token)

    def _has_grouped_assignment(self):
        if self._initializer_deduction == self._GROUPED_SUBSCRIPT:
            return self.last_token == "]"
        return self._initializer_deduction == self._GROUPED_OPERAND and self.last_token != "operator"

    def _read_grouped_assignment_rhs(self, token):
        self._set_state(self._state_global, self._IDLE_TOKENS)
        if token == "&&":
            # GNU C and C++ use unary && to take a label's address.
            if self._counts_and(token):
                self.context.add_condition(-1)
        else:
            self._state_global(token)

    def _lambda_head(self, token):
        # Keep the reference scan active in captures and parameter types.
        header = self._lambda
        adjustment = None
        if header.in_capture_prefix or not header.in_group or token in self._LAMBDA_GROUP_TOKENS:
            adjustment = header.advance(token, self.last_token)
        self._state, self._events = header.resume
        if token == "&&" and header.in_return_type and self._counts_and(token):
            header.return_reference_pending = True
            self._initializer_deduction = False
            self._set_state(self._r_value_ref, self._REFERENCE_SCAN_TOKENS)
        elif self._events is None or token in self._events:
            self._state(token)
        if adjustment is None:
            self._track_lambda(header)
        else:
            self._lambda = None
            if adjustment:
                self.context.add_condition(-adjustment)

    def _track_lambda(self, header):
        header.resume = self._state, self._events
        events = None
        if header.in_group and not header.in_capture_prefix:
            if self._events is self._IDLE_TOKENS:
                events = self._LAMBDA_IDLE_TOKENS
            elif self._events is self._REFERENCE_SCAN_TOKENS:
                events = self._LAMBDA_SCAN_TOKENS
        self._set_state(self._lambda_head, events)

    @CodeStateMachine.read_until_then(';')
    def _typedef(self, _, tokens):
        if "&&" in self._conditions and "&&" in tokens:
            self.context.add_condition(-sum(self._counts_and(token) for token in tokens if token == "&&"))
        self._set_state(self._state_global, self._IDLE_TOKENS)

    def _counts_and(self, token):
        if "&&" not in self._conditions:
            return False
        # Plain tokens have no extension metadata.
        return token.__class__ is str or not (
            getattr(token, 'is_ignored_condition', False) or getattr(token, 'is_member_name', False))


class _LambdaHeader:
    _GROUPS = {"[": "]", "(": ")", "{": "}", "<": ">"}
    _MODIFIERS = {"mutable", "consteval", "constexpr", "noexcept", "static"}
    _SUFFIX_SECTIONS = {"->", "requires", "requires-expression"}
    _SUFFIX_TOKENS = {"::", "*", "&", "&&", "||", "!", "..."}

    def __init__(self):
        self.resume = None
        self.references = 0
        self.return_reference_pending = False
        self._capture_has_initializer = False
        self._section = "["
        self._groups = ["]"]

    @property
    def in_parameter(self):
        return self._section == "(" and len(self._groups) == 1

    @property
    def in_return_type(self):
        return self._section == "->" and not self._groups

    @property
    def in_capture_prefix(self):
        return self._section == "[" and len(self._groups) == 1 and not self._capture_has_initializer

    @property
    def in_group(self):
        return bool(self._groups)

    def advance(self, token, previous):
        """Return None while tracking, or the deduction when tracking ends."""
        section = self._section
        groups = self._groups
        if not groups:
            return self._after_group(token, previous)
        if token == groups[-1]:
            groups.pop()
            if section == "(" and not groups:
                self._section = ")"
            return None
        at_outer_group = len(groups) == 1
        if self.in_capture_prefix:
            if token == "=":
                self._capture_has_initializer = True
            elif token in ("@", "[") or (
                    token.isidentifier() and (previous.isidentifier() or previous in (")", "}"))):
                # Objective-C receivers can contain literals or subscripts.
                return 0
        if token in (")", "]", "}") or (section == "(" and token == "="):
            return 0
        if at_outer_group and token == ";" and groups[-1] != "}":
            return 0
        if token in self._GROUPS:
            if token != "<" or groups[-1] == ">" or (section == "(" and at_outer_group):
                groups.append(self._GROUPS[token])
        return None

    def _after_group(self, token, previous):
        if self._section not in ("[", "<"):
            return self._after_parameters(token, previous)
        if token == "(" or (token == "<" and self._section == "["):
            self._section = token
            self._groups.append(self._GROUPS[token])
            return None
        if token == "->" or token in self._MODIFIERS:
            # C++23 permits these specifiers without a parameter list.
            self._section = ")"
            return self._after_parameters(token, previous)
        return 0

    def _after_parameters(self, token, previous):
        if self.return_reference_pending:
            # A block call followed by pointer member access can look like a lambda.
            self.return_reference_pending = False
            if token in ("{", "requires"):
                self.references += 1
        if token == "{":
            if self._section == "requires" and previous == "requires":
                return 0  # A trailing requires clause needs a constraint before the body.
            if self._section != "requires-expression":
                return self.references
            self._section = "requires"
            self._groups.append("}")
        elif token == "(" and previous == "noexcept":
            self._groups.append(")")
        elif token == "->" and self._section == ")":
            self._section = "->"
        elif token == "requires":
            self._section = "requires-expression" if self._section == "requires" else "requires"
        elif self._section in self._SUFFIX_SECTIONS:
            if token in self._GROUPS:
                self._groups.append(self._GROUPS[token])
            elif token not in self._SUFFIX_TOKENS and not token.isidentifier():
                return 0
        elif token not in self._MODIFIERS:
            return 0
        return None


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
