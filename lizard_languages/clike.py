'''
Language parser for C, C++ -like languages.
'''

import re
import itertools
from .code_reader import CodeStateMachine, CodeReader


class CCppCommentsMixin(object):  # pylint: disable=R0903

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith("/*") or token.startswith("//"):
            return token[2:]


# pylint: disable=R0903
class CLikeReader(CodeReader, CCppCommentsMixin):
    ''' This is the reader for C, C++ and Java. '''

    ext = ["c", "cpp", "cc", "mm", "cxx", "h", "hpp"]
    language_names = ['cpp', 'c']
    macro_pattern = re.compile(r"#\s*(\w+)\s*(.*)", re.M | re.S)

    def __init__(self, context):
        super(CLikeReader, self).__init__(context)
        self.parallel_states = (
                CLikeStates(context),
                CLikeNestingStackStates(context),
                CppRValueRefStates(context))

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
                    for _ in macro.group(2).splitlines()[1:]:
                        yield '\n'
                else:
                    yield token


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

    # Beasts that can be defined within one line without braces.
    __braceless_structures = set(['if', 'else', 'for', 'while', 'do',
                                  'switch'])
    __paren_count = 0  # Used only to tackle the beasts.
    __braceless = None  # Applies only to the beasts.
    __structure_brace_stack = []  # Boolean stack for structures' brace states.

    def __pop_braceless_structures(self):
        """Pops structures up to the one with braces."""
        self.context.pop_nesting()
        is_structure = None
        if self.__structure_brace_stack:
            is_structure = self.__structure_brace_stack.pop()

        while (is_structure is not None and self.__structure_brace_stack and
                self.__structure_brace_stack[-1]):
            self.__structure_brace_stack.pop()
            self.context.pop_nesting()

    def __else_if_structure(self, token):
        """Handles possible compound 'else if' after 'else' token."""
        self._state = self.__declare_structure
        if token != "if":
            self._state(token)

    def __declare_structure(self, token):
        """Ignores structures between parentheses on structure declaration."""
        if token == "(":
            self.__paren_count += 1
        elif token == ")":
            # assert self.__paren_count > 0
            self.__paren_count -= 1
        elif self.__paren_count == 0:
            self._state = self._state_global
            if token == "{":
                self.__braceless = False
            else:
                self.__braceless = True
                self.context.add_bare_nesting()
                self.__structure_brace_stack.append(True)
            self._state(token)

    def _state_global(self, token):
        """Dual-purpose state for global and structure bodies."""
        if token in ("struct", "class", "namespace"):
            self._state = self._read_namespace

        elif token == "{":
            self.context.add_bare_nesting()
            self.__structure_brace_stack.append(self.__braceless)
            self.__braceless = None

        elif token == '}' or (token == ";" and self.__structure_brace_stack and
                              self.__structure_brace_stack[-1]):
            self.__braceless = None
            self.__pop_braceless_structures()

        elif token in self.__braceless_structures:
            # assert self.__paren_count == 0
            if token == "else":
                self._state = self.__else_if_structure
            else:
                self._state = self.__declare_structure

    @CodeStateMachine.read_until_then('({;')
    def _read_namespace(self, token, saved):
        self._state = self._state_global
        if token == "{":
            self.context.add_namespace(''.join(itertools.takewhile(
                lambda x: x not in [":", "final"], saved)))


# pylint: disable=R0903
class CLikeStates(CodeStateMachine):
    ''' This is the reader for C, C++ and Java. '''
    parameter_bracket_open = '(<'
    parameter_bracket_close = ')>'

    def __init__(self, context):
        super(CLikeStates, self).__init__(context)
        self.bracket_stack = []
        self._saved_tokens = []

    def try_new_function(self, name):
        self.context.try_new_function(name)
        self._state = self._state_function
        if name == 'operator':
            self._state = self._state_operator

    def _state_global(self, token):
        if token[0].isalpha() or token[0] in '_~':
            self.try_new_function(token)

    def _state_function(self, token):
        if token == '(':
            self.next(self._state_dec, token)
        elif token == '::':
            self.context.add_to_function_name(token)
            self.next(self._state_name_with_space)
        elif token == '<':
            self.next(self._state_template_in_name, token)
        else:
            self.next(self._state_global, token)

    @CodeStateMachine.read_inside_brackets_then("<>", "_state_function")
    def _state_template_in_name(self, token):
        self.context.add_to_function_name(token)

    def _state_operator(self, token):
        if token != '(':
            self._state = self._state_operator_next
        self.context.add_to_function_name(' ' + token)

    def _state_operator_next(self, token):
        if token == '(':
            self._state_function(token)
        else:
            self.context.add_to_function_name(' ' + token)

    def _state_name_with_space(self, token):
        self._state = self._state_operator\
            if token == 'operator' else self._state_function
        self.context.add_to_function_name(token)

    @CodeStateMachine.read_inside_brackets_then("()", "_state_dec_to_imp")
    def _state_dec(self, token):
        if token in self.parameter_bracket_open:
            self.bracket_stack.append(token)
        elif token in self.parameter_bracket_close:
            if self.bracket_stack:
                self.bracket_stack.pop()
            else:
                self.next(self._state_global)
        elif len(self.bracket_stack) == 1:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(token)

    def _state_dec_to_imp(self, token):
        if token == 'const' or token == 'noexcept':
            self.context.add_to_long_function_name(" " + token)
        elif token == 'throw':
            self._state = self._state_throw
        elif token == '(':
            long_name = self.context.current_function.long_name
            self.try_new_function(long_name)
            self._state_function(token)
        elif token == '{':
            self.next(self._state_entering_imp, "{")
        elif token == ":":
            self._state = self._state_initialization_list
        elif not (token[0].isalpha() or token[0] == '_'):
            self._state = self._state_global
            self._state(token)
        else:
            self._state = self._state_old_c_params
            self._saved_tokens = [token]

    def _state_throw(self, token):
        if token == ')':
            self._state = self._state_dec_to_imp

    def _state_old_c_params(self, token):
        self._saved_tokens.append(token)
        if token == ';':
            self._saved_tokens = []
            self._state = self._state_dec_to_imp
        elif token == '{':
            if len(self._saved_tokens) == 2:
                self._saved_tokens = []
                self._state_dec_to_imp(token)
                return
            self._state = self._state_global
            for token in self._saved_tokens:
                self._state(token)
        elif token == '(':
            self._state = self._state_global
            for token in self._saved_tokens:
                self._state(token)

    def _state_initialization_list(self, token):
        self._state = self._state_one_initialization
        if token == '{':
            self.next(self._state_entering_imp, "{")

    @CodeStateMachine.read_until_then('({')
    def _state_one_initialization(self, token, _):
        if token == "(":
            self._state = self._state_initialization_value1
        else:
            self._state = self._state_initialization_value2
        self._state(token)

    @CodeStateMachine.read_inside_brackets_then("()")
    def _state_initialization_value1(self, _):
        self._state = self._state_initialization_list

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _state_initialization_value2(self, _):
        self._state = self._state_initialization_list

    def _state_entering_imp(self, token):
        self.context.confirm_new_function()
        self.next(self._state_imp, token)

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _state_imp(self, _):
        self._state = self._state_global
