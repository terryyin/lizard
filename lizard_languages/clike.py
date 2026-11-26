'''
Language parser for C, C++ -like languages.
'''

import re
import itertools
from .code_reader import CodeStateMachine, CodeReader
from .clike_states import CLikeStates, CPP_CONDITIONAL_DIRECTIVES

# Tokenizer fragment for C++ empty-delimiter raw strings R"(...)" (optional u8/u/U/L
# prefix). Stops at the first ')"'. Custom delimiters use the normal string rule.
# See #451, #478; subgroup-free per CodeReader.generate_tokens.
_CPP_RAW_STRING_TOKEN = r"|(?:u8|u|U|L)?R\"\((?:[^)]|\)(?!\"))*\)\""
# In C and C++, a backslash at the end of a line continues a // comment on the next line.
_C_LINE_COMMENT = r"|\/\/(?:\\\n|[^\n])*"


_ANGLE_CHANGE = {"<": 1, ">": -1}
# The preprocessor can join a tilde to the following delimiter.
_GROUP_CHANGE = {"(": 1, ")": -1, "~(": 1, "~)": -1, "[": 1, "]": -1, "~[": 1, "~]": -1}
_EXPRESSION_OPERATORS = frozenset((
    "+", "-", "*", "/", "%", "&", "|", "^", "&&", "||", "?", ":",
    "==", "!=", "<=", ">=", "!", "and", "or", "bitand", "bitor", "xor", "not", "not_eq",
))


class _CppTemplateAngles:
    _template_group_depth = 0
    _at_template_argument = False
    _after_template_group = False
    _in_grouped_expression = False
    _in_comparison_operand = False
    _template_braces = 0
    _grouped_angle = False

    def _read_template_angle(self, token):
        if self._after_template_group:
            if token == "<":
                # A parenthesized left operand cannot introduce another angle list.
                self._at_template_argument = True
                self._in_grouped_expression = True
                self._in_comparison_operand = True
                return False
            self._after_template_group = False
            if token in _EXPRESSION_OPERATORS:
                self._in_grouped_expression = True
                self._at_template_argument = True
                return False
        if self._template_group_depth:
            self._template_group_depth += _GROUP_CHANGE.get(token, 0)
            if token in _ANGLE_CHANGE:
                self._grouped_angle = True
            if not self._template_group_depth:
                self._after_template_group = True
        elif ((self._at_template_argument and token in ("(", "~(")) or
              (self._in_comparison_operand and token in ("[", "~["))):
            self._template_group_depth = 1
            self._at_template_argument = False
        elif token == "{":
            self._template_braces += 1
        elif token == "}" and self._template_braces:
            self._template_braces -= 1
        elif token in (";", "}") and self._grouped_angle and not self._template_braces:
            # A comparison such as width < (height > 0) can resemble a template.
            self.br_count = 0
            self._reset_template_angles()
            return True
        else:
            self.br_count += _ANGLE_CHANGE.get(token, 0)
            if self.br_count == 1 and token == ",":
                self._in_comparison_operand = False
                self._in_grouped_expression = False
            # A second '<' could be part of an ungrouped comparison.
            self._at_template_argument = (
                (token in ("<", "=", ",") and (self.br_count == 1 or self._in_comparison_operand))
                or (self._in_grouped_expression and self.br_count == 1 and token in _EXPRESSION_OPERATORS))
        if not self.br_count:
            self._reset_template_angles()
        return False

    def _reset_template_angles(self):
        self._at_template_argument = False
        self._after_template_group = False
        self._in_grouped_expression = False
        self._in_comparison_operand = False
        self._template_braces = 0
        self._grouped_angle = False


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
                CppStates(context),
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
                    directive = macro.group(1)
                    if directive in CPP_CONDITIONAL_DIRECTIVES:
                        for state in self.parallel_states:
                            if isinstance(state, CLikeStates):
                                state.read_preprocessor_branch(directive)
                                break
                    if directive in ('if', 'ifdef', 'elif'):
                        self.context.add_condition()
                    elif directive == 'include':
                        yield "#include"
                        yield macro.group(2) or "\"\""
                    for _ in macro.group(2).split('\n')[1:]:
                        yield '\n'
                else:
                    yield token


class CppStates(CLikeStates, _CppTemplateAngles):  # pylint: disable=R0903
    _initializer_type = None

    def _state_function(self, token):
        if token == "=":
            self._state = self._state_initializer_start
        else:
            super()._state_function(token)

    def _state_initializer_start(self, token):
        self._state = self._state_global
        self._state(token)
        if token in ("sizeof", "decltype"):
            self._initializer_type = self.context.current_function

    def _state_dec_to_imp(self, token):
        if token == "{" and self.context.current_function is self._initializer_type:
            # An operand in an initializer can resemble a function declaration.
            self._initializer_type = None
            self.br_count = 1
            self._state = self._state_initializer
        else:
            super()._state_dec_to_imp(token)

    def _state_template_in_name(self, token):
        if token == "<" and not self.br_count:
            self._state = self._state_template_start
        if self._read_template_angle(token):
            self._state = self._state_global
            return
        self.context.add_to_function_name(token)
        if not self.br_count:
            self._state = self._state_function

    def _state_template_start(self, token):
        self._state = self._state_shift_or_digraph if token == "<" else self._state_template_in_name
        self._state_template_in_name(token)

    def _state_shift_or_digraph(self, token):
        if token == "::":
            # This can qualify the shift operand or be part of '<::>'.
            self.context.add_to_function_name(token)
            return
        # The tokenizer joins ':=' when the capture starts with '<:='.
        if token in (":", ":=") or (token == ">" and self.context.current_function.name.endswith("::")):
            self._state = self._state_template_in_name
        else:
            self.br_count = 0
            self._reset_template_angles()
            self._state = self._state_initializer
        self._state(token)

    def _state_initializer(self, token):
        # A braced operand can contain semicolons, as in a lambda body.
        if token in (";", "}") and not self.br_count:
            self._state = self._state_global
        elif token == "{":
            self.br_count += 1
        elif token == "}":
            self.br_count -= 1


class CppRValueRefStates(CodeStateMachine):
    # pylint: disable=R0903

    def _state_global(self, token):
        if token == "&&":
            self.next(self._r_value_ref)
        elif token == "typedef":
            self.next(self._typedef)

    @CodeStateMachine.read_until_then('=;{})')
    def _r_value_ref(self, token, _):
        if token == "=":
            self.context.add_condition(-1)
        self.next(self._state_global)

    @CodeStateMachine.read_until_then(';')
    def _typedef(self, _, tokens):
        self.context.add_condition(-tokens.count("&&"))
        self.next(self._state_global)


# pylint: disable=R0903
class CLikeNestingStackStates(CodeStateMachine, _CppTemplateAngles):
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

    def _template_declaration(self, token):
        # A template parameter list can only start outside a function.
        if token == "<" and self.context.last_function is None:
            self._state = self._read_template_parameters
        else:
            self._state = self._state_global
        self._state(token)

    def _read_template_parameters(self, token):
        if self._read_template_angle(token):
            self._state = self._state_global
            self._state_global(token)
        elif not self.br_count:
            self._state = self._state_global

    @CodeStateMachine.read_inside_brackets_then("[]", "_read_namespace")
    def _read_attribute(self, _):
        """Ignores C++11 attributes inside [[ ]]."""
        pass
