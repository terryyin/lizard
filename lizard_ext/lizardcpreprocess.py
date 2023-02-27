'''
This is an extension of lizard,
It helps to deal with C code with preprocessors that
is hard to parse. It works by checking the first if
condition and deciding to take that or the final else
'''

import re

class LizardExtension(object):  # pylint: disable=R0903

    ordering_index = 0
    macro_pattern = re.compile(r"#\s*(\w+)\s*(.*)", re.M | re.S)

    def __call__(self, tokens, reader):
        def preprocess_tokens(tokens):
            if_stack        = []     # if-like directive with condition
            directive_stack = []     # current directive, unstacked on endif

            for token in tokens:
                macro = self.macro_pattern.match(token)
                if macro:
                    directive = macro.group(1)
                    _update_stacks(token, directive, if_stack, directive_stack)
                    yield from _blank_lines(token)
                elif directive_stack:
                    if_condition = if_stack[-1]
                    directive = directive_stack[-1]
                    yield from _handle_condition(token, if_condition, directive)
                else:
                    yield token

        def _update_stacks(token, directive, ifs, directives):
            if directive in ('if', 'ifdef', 'ifndef'):   # push on the stack
                ifs.append(token)
                directives.append('if')
            elif directive in ('elif', 'else'):          # replace the directive
                directives.pop()
                directives.append(directive)
            elif directive == 'endif':                   # pop from stack
                ifs.pop()
                directives.pop()

        def _filter_token(token, directive, keep):
            if directive == keep:
                return token
            return _blank_lines(token)

        def _handle_condition(token, if_condition, directive):
            keep = 'if' # default
            if if_condition.startswith("#if 0"): # skip if, take else
                keep = 'else'
            return _filter_token(token, directive, keep)

        def _blank_lines(token):
            for _ in range(token.count('\n')):
                yield '\n'

        if "c" not in reader.ext:
            return tokens
        return preprocess_tokens(tokens)
