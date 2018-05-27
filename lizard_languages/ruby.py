'''
Language parser for JavaScript
'''

from .code_reader import CodeReader, CodeStateMachine
from .script_language import ScriptLanguageMixIn
from .js_style_regex_expression import js_style_regex_expression


def state_embedded_doc(token):
    if token == "=end":
        return True


class RubyStateMachine(CodeStateMachine):
    # pylint: disable=R0903
    def is_newline(self):
        return self.context.newline or self.last_token == ";"

    def _state_global(self, token):
        if token in ("end", "}"):
            self.sm_return()
        elif token == 'def':
            self._state = self._def
            self.next(self._def)
        elif token == 'it':
            self._state = self._it
        elif token in ("begin", "do", "class", "module", "{", "${"
                       ) and self.last_token != ".":
            self.sub_state(RubyStateMachine(self.context))
        elif token in ("while", "for"):
            if self.is_newline():
                self.next(self._for_while)
        elif token in ("if", "unless"):
            if self.is_newline():
                self.sub_state(RubyStateMachine(self.context))
            else:
                self.next(self._if)
        elif token == "=begin":
            self.sub_state(state_embedded_doc)

    def _def(self, token):
        self.context.start_new_function(token)
        self.next(self._def_continue)

    def _it(self, token):
        if token in ('do', '{'):
            self.context.start_new_function(self.last_token)
            self.next(self._def_continue)

    def _def_continue(self, token):
        def callback():
            self.context.end_of_function()
            self.next(self._state_global)
        if token == ".":
            self.context.add_to_function_name(token)
            self.next(self._def_class_method)
        elif token == "(":
            self.next(self._def_parameters)
        else:
            self.sub_state(RubyStateMachine(self.context), callback, token)

    def _def_class_method(self, token):
        self.context.add_to_function_name(token)
        self.next(self._def_continue)

    def _def_parameters(self, token):
        if token == ')':
            self.next(self._def_continue)
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _if(self, token):
        if self.is_newline():
            self.next(self._state_global, token)
        elif token == "then":
            self.next(self._state_global)
            self.sub_state(RubyStateMachine(self.context))

    def _for_while(self, token):
        if self.is_newline() or token == "do":
            self.next(self._state_global)
            if token != "end":
                self.sub_state(RubyStateMachine(self.context))


class MyToken(str):
    def __new__(cls, value, *_):
        return super(MyToken, cls).__new__(cls, value.group(0))

    def __init__(self, value):
        super(MyToken, self).__init__()
        self.begin = value.start()


class RubyReader(CodeReader, ScriptLanguageMixIn):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']
    conditions = set(['if', 'until', 'for', 'while', 'and', 'or',
                      'elsif', 'rescue', 'ensure', 'when', '||', '&&', '?'])

    def __init__(self, context):
        super(RubyReader, self).__init__(context)
        self.parallel_states = [RubyStateMachine(context)]

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        def process_source(source, _, matcher):
            return ScriptLanguageMixIn.generate_common_tokens(
                source,
                r"|^\=begin|^\=end" +
                r"|\%[qQrw]?\{(?:\\.|[^\}\\])*?\}" +
                r"|\%[qQrw]?\[(?:\\.|[^\]\\])*?\]" +
                r"|\%[qQrw]?\<(?:\\.|[^\>\\])*?\>" +
                r"|\%[qQrw]?\((?:\\.|[^\>\\])*?\)" +
                r"|\w+:" +
                r"|\$\w+" +
                r"|\.+" +
                r"|:?\@{0,2}\w+\??\!?" +
                _, matcher)
        matcher = MyToken
        bracket_stack = []
        source = source_code
        while source is not None:
            for token in process_source(source, addition, matcher):
                if token == "{":
                    bracket_stack.append("{")
                elif token == "}":
                    if bracket_stack:
                        if bracket_stack.pop() == '#{':
                            source = '"'+source[token.begin + 1:]
                            yield token
                            break
                elif token.startswith('"'):
                    first, sep, _ = token.partition('#{')
                    if sep:
                        yield first + '"'
                        yield '${'  # because #will be regarded as comments
                        bracket_stack.append(sep)
                        source = source[token.begin + token.find(sep)+2:]
                        break
                yield token
            else:
                source = None
