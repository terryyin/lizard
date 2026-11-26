'''
Language parser for Apple Swift
'''

from itertools import compress

from . import swift
from .clike import CCppCommentsMixin
from .code_reader import CodeReader, CodeStateMachine, _previous_code
from .golike import GoLikeStates
from .swift import SwiftReplaceLabel


class KotlinReader(CodeReader, CCppCommentsMixin, SwiftReplaceLabel):
    # pylint: disable=R0903

    ext = ['kt', 'kts']
    language_names = ['kotlin']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = set()  # Kotlin uses 'when' expressions, not case
    _ternary_operators = {'?:'}  # Elvis operator
    # `flow.catch { }`, `Promise::catch`, `fun catch()` and
    # `fun <T> catch()` name a member, also across a line break.
    _BEFORE_MEMBER_NAME = frozenset(('.', '::', 'fun'))
    _MEMBER_ACCESS_SPANS_LINES = True
    _TYPE_PARAMETERS_BEFORE_NAME = True

    def __init__(self, context):
        super(KotlinReader, self).__init__(context)
        self._expression_bodies = []
        self._statement_ends = None
        self.parallel_states = [KotlinStates(context, expression_bodies=self._expression_bodies)]

    def preprocess(self, tokens):
        # Kotlin has optional semicolons, so the end of a line can end an
        # expression body. Ending it here, before the next token is counted,
        # counts that token in the enclosing scope.
        # A body starts after `=`, never at the start of a line, so lines
        # are tracked only while a body is open.
        bodies = self._expression_bodies
        line_start = False
        enders = KotlinExpressionBody.ENDERS
        statement_ends = self._statement_ends
        tokens = self._mark_labels(tokens)
        for index, token in enumerate(tokens):
            if bodies:
                if token == '\n':
                    line_start = True
                elif ((line_start or token in enders)
                      and not (line_start and token[:2] in ('//', '/*'))):
                    ended = False
                    while bodies and bodies[-1].ends_before(token, line_start, tokens, index):
                        bodies[-1].end()
                        ended = True
                    line_start = False
                    # A body that ends at a bracket or a comma is part
                    # of a statement that goes on.
                    if ended and statement_ends is not None and token not in enders:
                        statement_ends.before_token = True
                        yield token
                        statement_ends.before_token = False
                        continue
            elif line_start:
                line_start = False
            yield token
        while bodies:
            bodies[-1].end()

    def _mark_labels(self, tokens):
        tokens = self.mark_member_names(
            tokens, self._BEFORE_MEMBER_NAME,
            line_breaks=self._MEMBER_ACCESS_SPANS_LINES,
            type_parameters=self._TYPE_PARAMETERS_BEFORE_NAME)
        labels = {token for token in self.conditions if token.isalpha()}
        labels.update(self._DECLARATION_LABELS)
        # Keyword labels are rare. Check their neighbors only at their positions.
        for index in compress(range(len(tokens)), map(labels.__contains__, tokens)):
            previous = _previous_code(tokens, index, line_breaks=True)
            if previous < 0 or tokens[previous] not in ('(', ','):
                continue
            colon = swift._next_significant(tokens, index + 1)
            if colon is not None and swift._is_local_name(tokens[colon]):
                colon = swift._next_significant(tokens, colon + 1)
            if colon is not None and tokens[colon] == ':':
                tokens[index] = '_' + tokens[index]
        return tokens

    def implied_semicolons(self, tokens):
        if self._statement_ends is None:
            self._statement_ends = _ExpressionBodyEnds()
        return tokens, self._statement_ends

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return CodeReader.generate_tokens(
            source_code,
            r"|`\w+`" +
            r"|\w+\?" +
            r"|\w+\!!" +
            r"|\?\?" +
            r"|\?:" +
            addition
        )


class KotlinStates(GoLikeStates):  # pylint: disable=R0902,R0903

    FUNC_KEYWORD = 'fun'
    TYPE_KEYWORD = None
    _ACCESSORS = {'get', 'set'}
    _BEFORE_ACCESSOR_IDENTIFIER = {'.', '::', '@', ':', '=', 'typealias'}
    _ACCESSOR_MODIFIERS = {'public', 'protected', 'internal', 'private', 'inline', 'external'}
    # The tokens that `_state_global` acts on.
    _GLOBAL_TOKENS = _ACCESSORS | {
        'class', 'object', '->', 'val', 'var', ',', 'interface', 'when', 'fun', '[', '{', '}',
    }
    _EXPRESSION_BODY_ENDERS = {
        'fun', 'class', 'interface', 'val', 'var',
    }
    _TYPE_PUNCTUATION = {'.', '?', '<', '>', ',', '(', ')', '->', '*'}
    # Words that start a declaration when they begin a line.
    _DECLARATION_STARTS = {
        '@', 'fun', 'val', 'var', 'class', 'interface', 'object', 'typealias',
        'init', 'constructor', 'companion', 'enum', 'data', 'sealed', 'annotation',
        'inner', 'value', 'open', 'abstract', 'final', 'override', 'lateinit',
        'const', 'suspend', 'inline', 'operator', 'infix', 'tailrec', 'expect',
        'actual', 'context', 'import', 'package',
    } | _ACCESSORS | _ACCESSOR_MODIFIERS
    _HEADER_OPERATORS = {':', 'where', '.', '->', ',', '<', '(', '&', 'suspend'}

    def __init__(self, context, in_when_cases=False, accessor_declaration_scope=None,
                 expression_bodies=None):
        super().__init__(context)
        self._in_when_cases = in_when_cases
        self._accessor_name = None
        self._accessor_declaration_scope = (
            accessor_declaration_scope or context.global_pseudo_function
        )
        self._expression_bodies = [] if expression_bodies is None else expression_bodies
        self._start_header()

    def statemachine_clone(self):
        return KotlinStates(self.context, expression_bodies=self._expression_bodies)

    def _state_global(self, token):
        if token not in self._GLOBAL_TOKENS:
            return
        if (token in self._ACCESSORS and self.last_token not in self._BEFORE_ACCESSOR_IDENTIFIER
                and self.context.current_function is self._accessor_declaration_scope):
            self._start_header()
            self._accessor_name = token
            self.context.push_new_function(token)
            self._state = self._expect_function_impl
        elif (token in ('class', 'object') and self.last_token != '::'
              and self.context.current_function is not self.context.global_pseudo_function):
            self._state = self._local_class_header
        elif token == '->':
            if self._in_when_cases:
                self.context.add_condition()
            else:
                self._state = self._function_type_result
        elif token in ('val', 'var', ',', 'interface'):
            self._state = self._expect_declaration_name
        elif token == 'when':
            self._state = self._when_cases
        else:
            if token == self.FUNC_KEYWORD:
                self._start_header()
                self._accessor_name = None
            super(KotlinStates, self)._state_global(token)

    def _local_class_header(self, token):
        if token == '(':
            self.next(self._local_class_parameters, token)
        elif token == '{':
            self._read_local_class_body()
        elif token in self._EXPRESSION_BODY_ENDERS or token in ('}', ';'):
            self.next(self._state_global, token)

    def _read_local_class_body(self):
        context = self.context
        enclosing_forgiveness = context.forgive, context.forgive_directive

        def restore_forgiveness():
            context.forgive, context.forgive_directive = enclosing_forgiveness

        # The class has its own declarations while the enclosing function
        # remains active for metrics.
        context.forgive = False
        context.forgive_directive = None
        self.next(self._state_global)
        self.sub_state(KotlinStates(
            context, accessor_declaration_scope=context.current_function,
            expression_bodies=self._expression_bodies,
        ), restore_forgiveness)

    @CodeStateMachine.read_inside_brackets_then("()", "_local_class_header")
    def _local_class_parameters(self, token):
        pass

    def _expect_declaration_name(self, token):
        # A name after `val`, `var` or `,` is not a keyword: `val type`,
        # `(name, set)`. A lambda or a `when` argument, a parameter
        # (`, val x`) and the `->` of a `when` branch keep their role.
        if token in ('{', 'when', 'val', 'var') or (
                token == '->' and self._in_when_cases):
            self.next(self._state_global, token)
        else:
            self._state = self._state_global

    def _function_type_result(self, token):
        # Only a function type with a lambda initializer starts a function:
        # `val f: () -> Unit = { ... }`. The arrow of a lambda parameter list
        # or of a type with no lambda does not.
        if token == '=':
            self._state = self._lambda_initializer
        elif token not in self._TYPE_PUNCTUATION and not token[0].isupper():
            self.next(self._state_global, token)

    def _lambda_initializer(self, token):
        if token == '{':
            self.context.push_new_function("(anonymous)")
            self.next(self._function_impl, token)
        else:
            self.next(self._state_global, token)

    def _expect_function_impl(self, token):
        if token == '=':
            self._state = self._state_global
            self.sub_state(KotlinExpressionBody(self), self.context.end_of_function)
        elif token == '{':
            self.next(self._function_impl, token)
        elif self._ends_declaration(token):
            # `abstract fun f(): Int` or `private set` has no body. Its first
            # line and token were already counted in the enclosing scope.
            self.fold_into_enclosing_function()
            self.context.current_function.nloc -= 1
            self.context.current_function.token_count -= 1
            self.next(self._state_global, token)

    def _finish_declaration_forgiveness(self, token):
        context = self.context
        if not context.forgive or context.forgive_directive is None:
            return
        owner, line, token_count, follows_code = context.forgive_directive
        declaration = context.current_function
        if owner is self._accessor_declaration_scope and owner is context.stacked_functions[-1]:
            context.forgive = False
        elif owner is declaration:
            # A comment between two accessors can introduce the next one
            # even when both accessors are on the same line.
            next_accessor = token in self._ACCESSORS and line == context.current_line
            # The boundary token already counts. Any earlier token after
            # the comment still belonged to this declaration.
            more_header_tokens = context.fileinfo.token_count > token_count + 1
            if (follows_code or more_header_tokens) and not next_accessor:
                context.forgive = False
            else:
                context.forgive_directive = (context.stacked_functions[-1], line, token_count, follows_code)

    def _ends_declaration(self, token):
        # Kotlin semicolons are optional, so a declaration without a body
        # also ends where the next declaration starts a line, or where the
        # other accessor follows it: `private set public get() { }`.
        ends = token in (';', '}') or (not self._type_is_open() and (
            (self.context.newline and token in self._DECLARATION_STARTS)
            or (token in self._ACCESSORS and token != self._accessor_name
                and (self.last_token == self._accessor_name
                     or self.last_token in self._ACCESSOR_MODIFIERS))))
        self._track_type(token)
        if ends:
            self._finish_declaration_forgiveness(token)
        return ends

    def _start_header(self):
        self._type_depth = 0
        self._type_annotated = False

    def _type_is_open(self):
        # The next token continues a type that has an open `<` or `(`, that
        # follows an operator, or that follows an annotation or a context
        # receiver: `List<out`, `T &`, `@A`, `context(String)`.
        return (self._type_depth > 0 or self._type_annotated
                or self.last_token in self._HEADER_OPERATORS)

    def _track_type(self, token):
        # An annotation keeps its role through a qualified name and its
        # arguments: `@a.B(1)`.
        if token in ('<', '('):
            if self._type_depth == 0 and self.last_token == 'context':
                self._type_annotated = True
            self._type_depth += 1
        elif token in ('>', ')'):
            self._type_depth = max(0, self._type_depth - 1)
        elif self._type_depth == 0:
            self._type_annotated = self.last_token == '@' or (
                self._type_annotated and '.' in (token, self.last_token))

    def _function_name(self, token):
        if token == "<":
            self.next(self._template, token)
        elif token == "@":
            self._state = self._receiver_annotation
        elif not self._receiver_type_end(token):
            return super(KotlinStates, self)._function_name(token)

    def _receiver_annotation(self, _):
        self._state = self._function_name

    def _expect_function_dec(self, token):
        if not self._receiver_type_end(token):
            super()._expect_function_dec(token)

    def _receiver_type_end(self, token):
        # In `fun String.f()` and `fun List<T>?.f()`, the name read so far
        # is the receiver type. The function name comes after the dot.
        # A type argument list with a `?`, such as `<T?>`, is one token.
        if token == '.':
            self.context.current_function.name = ''
            self.context.current_function.long_name = ''
            self._state = self._function_name
        return token in ('.', '?') or token[:1] == '<' and token[-1:] == '>' and len(token) > 1

    @CodeStateMachine.read_inside_brackets_then("<>", "_function_name")
    def _template(self, tokens):
        pass

    def _when_cases(self, token):
        def callback():
            self.context.add_condition(inc=-1)
            self.next(self._state_global)
        if token != '{':
            return
        self.sub_state(KotlinStates(
            self.context, in_when_cases=True, expression_bodies=self._expression_bodies,
        ), callback)


class KotlinExpressionBody(KotlinStates):  # pylint: disable=R0902,R0903
    '''Reads an expression body like a block body. The body ends before a
    bracket or a semicolon that it did not open, or at the end of a line
    where the expression does not continue.'''

    ENDERS = {'}', ')', ']', ';', ',', 'else', '->'}
    # The Kotlin grammar continues an expression on the next line after
    # these tokens...
    _CONTINUES_AT_LINE_END = {
        '=', '.', '::', ',', ':', '->', '+', '-', '*', '/', '%', '&&', '||',
        '?:', '==', '!=', '===', '!==', '<', '<=', '>=', 'in', 'is', 'as', 'as?',
        '@', 'if', 'else', 'when', 'object', 'throw',
    }
    # ...and before these ones (`?` starts `?.`).
    _CONTINUES_AT_LINE_START = {
        '.', '?', '?:', '&&', '||', 'as', 'as?', 'catch', 'finally',
    }
    # A block can follow these keywords across a line break.
    _TAKES_BLOCK_ON_NEXT_LINE = {'when', 'try', 'finally', 'do'}
    # Tokens that can follow a `<` in type arguments, `Map<K, List<V>?>`.
    _TYPE_ARGUMENT_TOKENS = {'<', '>', '.', ',', '?', '*', '@', ':', '->', '&', 'in', 'out'}
    _TYPE_GROUP_PREFIXES = {'<', ',', '(', '->', ':', '.', '&', 'in', 'out', 'suspend', 'context'}
    _TYPE_OPERATORS = {'as', 'as?', 'is'}
    _TYPE_PREFIXES = _TYPE_OPERATORS | {'.', '->', 'suspend'}
    # Words that are not operands, so a name after them is not an infix call.
    _KEYWORDS = {
        'if', 'else', 'when', 'try', 'catch', 'finally', 'return', 'throw',
        'is', 'as', 'as?', 'in', 'out', 'by', 'where', 'val', 'var', 'fun',
        'object', 'class', 'interface', 'typealias',
    }
    _BRACKET_CHANGES = {'(': 1, '[': 1, ')': -1, ']': -1}
    _type_argument_end = -1

    def __init__(self, parent):
        super().__init__(parent.context, expression_bodies=parent._expression_bodies)
        self._parent = parent
        self._function = parent.context.current_function
        # Only an anonymous function can be an argument: `f(fun() = 1, 2)`.
        self._anonymous = not self._function.name
        self._depth = 0
        self._condition = False
        self._closes_condition = False
        self._open_types = 0
        self._closes_type = False
        self._type_operand = False
        # What can come next in a type argument: 'type' right after `<`,
        # and 'dot' after a package name, `List<java.time.Instant>`.
        self._type_part = None
        self._open_ifs = 0
        # Between `object` and the `{` of its body: `object : Runnable`.
        self._object_header = False
        self._token_before_last = None
        # The body can start with annotations: `= @Suppress("X")`.
        self._annotation = '@'
        self._infix = False
        self._trailing_lambda_start = -1
        self.last_token = '='
        self._expression_bodies.append(self)

    def __call__(self, token, reader=None):
        state = self._state
        if isinstance(state, CodeStateMachine):
            return super().__call__(token, reader)
        if self._depth:
            self._depth += self._BRACKET_CHANGES.get(token, 0)
            self._closes_condition = self._depth == 0 and self._condition
        else:
            self._track_top_level(token)
        state(token)
        self.last_token = token
        if self.to_exit:
            return True

    def next(self, state, token=None):
        # A state that passes its token on to the next state must not count
        # that token's brackets twice.
        self._state = state
        if token is not None:
            return CodeStateMachine.__call__(self, token)
        return None

    def _function_type_result(self, token):
        if self._open_types:
            # An arrow inside type arguments belongs to the type, so it
            # cannot introduce a lambda initializer elsewhere in the body.
            self.next(self._state_global, token)
        else:
            super()._function_type_result(token)

    def _track_top_level(self, token):
        if self._annotation is not None or token == '@':
            self._track_annotation(token)
        # An infix name can take its argument across a line break.
        previous = self.last_token
        self._infix = self._is_name(token) and not self._infix and (
            self._is_name(previous)
            or previous[0] in '"\'0123456789'
            or previous in (']', '}')
            or previous == ')' and not self._closes_condition
            or previous == '!' and self._token_before_last == '!')
        self._closes_condition = False
        self._closes_type = False
        # A trailing lambda can attach to a generic call, such as
        # `run<Int>`. A type after a cast or type check accepts no lambda.
        first = token[0]
        if token in self._TYPE_OPERATORS:
            self._type_operand = True
        elif (self._type_operand and not self._open_types
              and first != '<' and token != '.'
              and self.last_token not in self._TYPE_PREFIXES):
            self._type_operand = token == '->' and self.last_token == ')'
        if first in '<>' or self._open_types:
            self._track_types(token)
        if token in ('(', '['):
            # `if (ready)` needs its branch, even with a line break.
            self._condition = self.last_token == 'if'
            self._depth = 1
        elif token in ('object', '{'):
            self._object_header = token == 'object'
        elif token == 'if':
            self._open_ifs += 1
        elif token == 'else' and self._open_ifs:
            self._open_ifs -= 1
        self._token_before_last = self.last_token

    def _track_annotation(self, token):
        # A line of annotations, like `@Suppress("UNCHECKED_CAST")`, annotates
        # the expression on the next line. `_annotation` holds what the line
        # can take next while it has only annotations, and None otherwise.
        if self.context.newline:
            self._annotation = '@'
        if self._annotation is None:
            return
        if token == '@' and self._annotation in ('@', 'more', 'args'):
            self._annotation = 'name'
        elif self._annotation == 'name' and token == '[':
            # A group such as `@[A B]` also annotates the next expression.
            self._annotation = 'args'
        elif self._annotation == 'name' and (token[0].isalpha() or token[0] in '_`'):
            self._annotation = 'more'
        elif self._annotation == 'more' and token in ('.', ':'):
            self._annotation = 'name'
        elif self._annotation == 'more' and token == '(':
            # The arguments of the annotation. A `(` after them starts the
            # expression: `@Suppress("X") (x as T)`.
            self._annotation = 'args'
        else:
            self._annotation = None

    def _is_name(self, token):
        return (token[0].isalpha() or token[0] in '_`') and token not in self._KEYWORDS

    def ends_before(self, token, line_start, tokens, index):
        if (self._depth or isinstance(self._state, CodeStateMachine)
                or self.context.current_function is not self._function
                or index <= self._trailing_lambda_start):
            return False
        if token in ('}', ')', ']', ';'):
            return True
        if token == 'else':
            # The `else` of an `if` around the body: `if (a) fun() = 1 else`.
            return not self._open_ifs
        if token == '->':
            # A direct anonymous condition can end at the arm separator.
            # A cast such as `value as (Int) -> Boolean` owns its type arrow.
            return (self._anonymous and self._parent._in_when_cases
                    and not (self._type_operand and self.last_token == ')')
                    and self._outside_type_arguments(tokens, index))
        if token == ',':
            # Not a comma between type arguments, `Map<K, V>`, or between
            # the supertypes of an `object :` expression.
            return (self._anonymous and self._state == self._state_global  # pylint: disable=W0143
                    and self._outside_type_arguments(tokens, index))
        return line_start and not (
            self._closes_condition
            or token == '(' and self.last_token == 'catch' and self._token_before_last == '}'
            or self.last_token == '>' and not self._closes_type
            or token == '>' and self._in_type_arguments()
            or self._annotation in ('more', 'args')
            or self._infix
            or self.last_token in self._CONTINUES_AT_LINE_END
            or self._ends_with_prefix_not()
            or token in self._CONTINUES_AT_LINE_START
            or self._continues_with_block(token, tokens, index))

    def _outside_type_arguments(self, tokens, index):
        if not self._open_types:
            return True
        # The tokens before the boundary can be either a comparison with a
        # constant or the start of a type list. Check the closing angle
        # once and reuse its position for later boundaries in that list.
        if index >= self._type_argument_end:
            self._type_argument_end = self._find_type_argument_end(tokens, index)
        return index >= self._type_argument_end

    def _find_type_argument_end(self, tokens, index):
        depth = self._open_types
        groups = []
        previous = tokens[index]
        index = swift._next_significant(tokens, index + 1)
        while index is not None:
            token = tokens[index]
            if token == '@':
                index = self._after_annotations(tokens, index)
                continue
            if token == '(':
                # A group can start a type or follow its modifiers. A name
                # followed by parentheses, such as abs(value), is a call.
                if previous not in self._TYPE_GROUP_PREFIXES:
                    break
                groups.append(previous)
            elif token == ')':
                if not groups:
                    break
                prefix = groups.pop()
                if prefix in ('context', 'suspend'):
                    index = swift._next_significant(tokens, index + 1)
                    if index is None:
                        break
                    if prefix == 'suspend' and tokens[index] != '->':
                        break
                    if prefix == 'context' and tokens[index] in (',', '>', ')'):
                        break
                    previous = prefix if prefix == 'context' else token
                    continue
            elif token == '<':
                depth += 1
            elif token == '>':
                depth -= 1
                if not depth:
                    return index if not groups and self._has_type_argument_suffix(tokens, index) else -1
            elif (token not in self._TYPE_ARGUMENT_TOKENS
                  and not (self._is_name(token) and token not in ('true', 'false', 'null'))
                  and not (token[0] == '<' and token[-1] == '>')):
                break
            previous = token
            index = swift._next_significant(tokens, index + 1)
        return -1

    def _has_type_argument_suffix(self, tokens, index):
        if self._type_operand:
            return True
        suffix = swift._next_significant(tokens, index + 1)
        if suffix is None:
            return False
        return (tokens[suffix] in ('(', '{', '.', '?', '::', '@')
                or self._is_name(tokens[suffix]) and suffix + 1 < len(tokens) and tokens[suffix + 1] == '@')

    def _continues_with_block(self, token, tokens, index):
        takes_lambda = not self._type_operand and (self.last_token == ')' or self._closes_type)
        if token == '{':
            return takes_lambda or self._object_header or self.last_token in self._TAKES_BLOCK_ON_NEXT_LINE
        if not takes_lambda:
            return False
        if token == '@':
            index = self._after_annotations(tokens, index)
            if index is None:
                return False
            token = tokens[index]
        # `fold(0)\n add@{ ... }` still has a trailing lambda. The label
        # can also start another statement, such as `add@ for (...)`.
        if token != '{':
            if (not (token[0].isalpha() or token[0] in '_`')
                    or index + 1 == len(tokens) or tokens[index + 1] != '@'):
                return False
            index = swift._next_significant(tokens, index + 2)
            if index is None or tokens[index] != '{':
                return False
        # These tokens belong to the lambda prefix, even across line breaks.
        self._trailing_lambda_start = index
        return True

    def _after_annotations(self, tokens, index):
        # A trailing lambda may have annotations. A declaration after the
        # expression can also have annotations, so check what follows them.
        while index is not None and tokens[index] == '@':
            index = swift._next_significant(tokens, index + 1)
            if index is None:
                break
            if tokens[index] != '[':
                index = swift._next_significant(tokens, index + 1)
                while index is not None and tokens[index] == '.':
                    name = swift._next_significant(tokens, index + 1)
                    if name is None:
                        return None
                    index = swift._next_significant(tokens, name + 1)
            if index is not None and tokens[index] in ('(', '['):
                index = self._after_annotation_group(tokens, index)
        return index

    @staticmethod
    def _after_annotation_group(tokens, index):
        opening = tokens[index]
        closing = ')' if opening == '(' else ']'
        depth = 1
        for part in range(index + 1, len(tokens)):
            if tokens[part] == opening:
                depth += 1
            elif tokens[part] == closing:
                depth -= 1
                if depth == 0:
                    return swift._next_significant(tokens, part + 1)
        return None

    def _track_types(self, token):
        # `<` and `>` open and close type arguments, `Map<K, V>`, or
        # compare. A `>` with no `<` to close compares, and so does a
        # `<` before a token that a type cannot have, `a < 0`.
        if token == '<':
            self._open_types += 1
            self._type_part = 'type'
        elif token == '>' and self._open_types:
            self._open_types -= 1
            self._closes_type = True
            self._type_part = None
        elif token[0] == '<' and token[-1] == '>':
            # The tokenizer packs a nullable type argument as `<String?>`.
            self._closes_type = True
        elif self._open_types and not self._continues_type_arguments(token):
            self._open_types = 0
            self._type_part = None

    def _in_type_arguments(self):
        # A lowercase name right after `<` is a package only if a `.`
        # follows it. Until then it can be the end of `value < limit`.
        return self._open_types and self._type_part != 'dot'

    def _continues_type_arguments(self, token):
        expected, self._type_part = self._type_part, None
        if expected == 'type':
            # A type starts with a capital letter by convention, unlike the
            # name in a comparison: `List<Int>`, but `value < limit`. A
            # lowercase name can be a package before the type.
            if self._is_name(token) and token[0].islower():
                self._type_part = 'dot'
                return True
            return token[0].isupper() or token in ('*', 'in', 'out', '@')
        if expected == 'dot':
            self._type_part = 'type'
            return token == '.'
        # The tokenizer reads `<Int?>` as one token.
        return (token in self._TYPE_ARGUMENT_TOKENS or token in ('(', '[')
                or self._is_name(token)
                or token[0] == '<' and token[-1] == '>' and len(token) > 2)

    def _ends_with_prefix_not(self):
        # `!` before a line break negates the next line, but `]!!` (which
        # the tokenizer splits into `!` `!`) is a complete expression.
        return self.last_token == '!' and self._token_before_last != '!'

    def end(self):
        self._expression_bodies.pop()
        parent = self._parent
        parent.next(parent.saved_state)
        callback, parent.callback = parent.callback, None
        callback()
        # The enclosing function reaches at least as far as the function in
        # it: `fun f() = fun() =` and the next line.
        enclosing = self.context.current_function
        enclosing.end_line = max(enclosing.end_line, self._function.end_line)

    def statemachine_before_return(self):
        # A `}` that the body could not match ended it.
        if self in self._expression_bodies:
            self._expression_bodies.remove(self)


class _ExpressionBodyEnds:  # pylint: disable=R0903
    '''An expression body ends without a `;` or a `}`. The extensions that
    count nested structures and nesting depth read a `;` there.'''

    before_token = False
