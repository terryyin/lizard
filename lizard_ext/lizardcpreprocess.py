'''
This is an extension of lizard,
It helps to deal with C code with preprocessors that
is hard to parse. It works by checking the first if
condition and deciding to take that or the final else
'''

from pprint import pprint
import re

class LizardExtension(object):  # pylint: disable=R0903

    ordering_index = 0
    macro_pattern = re.compile(r"#\s*(\w+)\s*(.*)", re.M | re.S)

    def __call__(self, tokens, reader):
        def preprocess_tokens(tokens):
            if_stack   = [] # if's with condition
            part_stack = [] # current parts, unstacked on endif

            for token in tokens:
                # define -> store key / value 
                # ifdef = if defined
                # ifndef = if !defined
                # #if #elif #else #endif
                macro = self.macro_pattern.match(token)
                if macro:
                    op = macro.group(1)
                    if op in ('if', 'ifdef', 'ifndef'):
                        if_stack.append(token)
                        part_stack.append(op)
                    elif op in ('elif', 'else'):
                        part_stack.pop()
                        part_stack.append(op)
                    elif op == 'endif':
                        if_stack.pop()
                        part_stack.pop()

                    for _ in range(token.count('\n')):
                        yield '\n'
                elif part_stack:
                    if_condition = if_stack[-1]
                    part = part_stack[-1]
                    if if_condition.startswith("#if 0"): # skip if, take else
                        if part in ('if', 'ifdef', 'ifndef'):
                            for _ in range(token.count('\n')):
                                yield '\n'
                        elif part == 'else':
                            yield token
                    else:                                # take if, skip else
                        if part in ('if', 'ifdef', 'ifndef'):
                            yield token
                        elif part == 'else':
                            for _ in range(token.count('\n')):
                                yield '\n'
                    # always skip elif's
                    if part == 'elif':
                        for _ in range(token.count('\n')):
                            yield '\n'
                else:
                    yield token

        if "c" not in reader.ext:
            return tokens
        tokens = preprocess_tokens(tokens)
        for token in tokens:
            pprint(token)
        return tokens
