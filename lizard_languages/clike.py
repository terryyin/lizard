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

    __structures = set(["if", "else", "for", "while", "do", "switch",
                        "try", "catch"])
    # Structures paired on the same nesting level.
    __paired_structures = {"if": "else", "try": "catch", "catch": "catch",
                           "do": "while"}
    __wait_for_pair = False  # Wait for the pair structure to close the level.
    __structure_brace_stack = []  # Structure and brace states.

    def __pop_without_pair(self):
        """Continue poping nesting levels without the pair."""
        self.__wait_for_pair = False
        while (self.__structure_brace_stack and
               self.__structure_brace_stack[-1]):
            structure = self.__structure_brace_stack.pop()
            self.context.pop_nesting()
            if structure in self.__paired_structures:
                self.__wait_for_pair = self.__paired_structures[structure]
                return

    def __pop_structures(self):
        """Pops structures up to the one with braces or a waiting pair."""
        self.context.pop_nesting()
        structure = None
        if self.__structure_brace_stack:
            structure = self.__structure_brace_stack.pop()

        if structure is None:
            return
        if structure in self.__paired_structures:
            self.__wait_for_pair = self.__paired_structures[structure]
            return
        self.__pop_without_pair()

    def __else_if_structure(self, token):
        """Handles possible compound 'else if' after 'else' token."""
        self._state = self.__declare_structure
        if token == "if":
            self.__structure_brace_stack[-1] = "if"
        else:
            self._state(token)

    @CodeStateMachine.read_inside_brackets_then("()")
    def __declare_structure(self, token):
        """Ignores structures between parentheses on structure declaration."""
        self.context.add_bare_nesting()
        self._state = self._state_structure
        if token != ")":
            self._state(token)

    def _state_structure(self, token):
        """Control-flow structure states right before the body."""
        self._state = self._state_global
        if token == "{":
            self.context.add_bare_nesting()
            self.__structure_brace_stack.append(False)
        else:
            self._state(token)

    def _state_global(self, token):
        """Dual-purpose state for global and structure bodies."""
        while self.__wait_for_pair and token != self.__wait_for_pair:
            self.__pop_without_pair()

        if token == "template":
            self._state = self._template_declaration

        elif token in ("struct", "class", "namespace"):
            self._state = self._read_namespace

        elif token == "{":
            self.context.add_bare_nesting()
            self.__structure_brace_stack.append(None)  # Non-structure braces.

        elif token == '}' or (token == ";" and self.__structure_brace_stack and
                              self.__structure_brace_stack[-1]):
            self.__pop_structures()

        elif token in self.__structures:
            self.__wait_for_pair = False
            self.__structure_brace_stack.append(token)
            if token == "else":
                self._state = self.__else_if_structure
            else:
                self._state = self.__declare_structure

    def _read_namespace(self, token):
        """Processes declarations right after namespace/class keywords."""
        if token == "[":
            self._state = self._read_attribute
        else:
            self._state = self._read_namespace_name
        self._state(token)

    @CodeStateMachine.read_until_then(')({;')
    def _read_namespace_name(self, token, saved):
        """Processes namespace/class/struct names from declrations."""
        self._state = self._state_global
        if token == "{":
            self.context.add_namespace(''.join(itertools.takewhile(
                lambda x: x not in [":", "final", "["], saved)))

    @CodeStateMachine.read_inside_brackets_then("<>", "_state_global")
    def _template_declaration(self, _):
        """Ignores template parameters."""
        pass

    @CodeStateMachine.read_inside_brackets_then("[]", "_read_namespace")
    def _read_attribute(self, _):
        """Ignores C++11 attributes inside [[ ]]."""
        pass


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
        if token in ('const', '&', '&&'):
            self.context.add_to_long_function_name(" " + token)
        elif token == 'throw':
            self._state = self._state_throw
        elif token == '->':
            self._state = self._state_trailing_return
        elif token == 'noexcept':
            self._state = self._state_noexcept
        elif token == '(':
            long_name = self.context.current_function.long_name
            self.try_new_function(long_name)
            self._state_function(token)
        elif token == '{':
            self.next(self._state_entering_imp, "{")
        elif token == ":":
            self._state = self._state_initialization_list
        elif token == "[":
            self._state = self._state_attribute
            self._state(token)
        elif not (token[0].isalpha() or token[0] == '_'):
            self._state = self._state_global
            self._state(token)
        else:
            self._state = self._state_old_c_params
            self._saved_tokens = [token]

    @CodeStateMachine.read_inside_brackets_then("()")
    def _state_throw(self, _):
        self._state = self._state_dec_to_imp

    def _state_noexcept(self, token):
        if token == '(':
            self._state = self._state_throw
        else:
            self._state = self._state_dec_to_imp
        self._state(token)

    @CodeStateMachine.read_until_then(';{')
    def _state_trailing_return(self, token, _):
        self._state = self._state_dec_to_imp
        self._state(token)

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

    @CodeStateMachine.read_inside_brackets_then("[]", "_state_dec_to_imp")
    def _state_attribute(self, _):
        "Ignores function attributes with C++11 syntax, i.e., [[ attribute ]]."
        pass
