'''
This is an extension of lizard,
It helps to deal with C code with preprocessors that
is hard to parse. It works by always ignoring the code
between #else and #end.
'''

from pprint import pprint

class LizardExtension(object):  # pylint: disable=R0903

    ordering_index = 0

    def __call__(self, tokens, reader):
        def preprocess_tokens(tokens):
            if_stack   = [] # if's with condition
            part_stack = [] # current parts, unstacked on endif

            for token in tokens:
                # define -> store key / value 
                # ifdef = if defined
                # ifndef = if !defined
                # #if #elif #else #endif
                if token.startswith("#"):
                    if token.startswith('#if'):
                        if_stack.append(token)
                        part_stack.append(token)

                    if token.startswith('#elif') or token.startswith('#else'):
                        part_stack.pop()
                        part_stack.append(token)

                    if token.startswith("#endif"):
                        if_stack.pop()
                        part_stack.pop()

                    for _ in range(token.count('\n')):
                        yield '\n'
                elif part_stack:
                    part = part_stack[-1]
                    if part.startswith("#else"):
                        for _ in range(token.count('\n')):
                            yield '\n'
                    if part.startswith("#if"):
                        yield token
                    if part.startswith("#elif"):
                        for _ in range(token.count('\n')):
                            yield '\n'
                else:
                    yield token

        if "c" not in reader.ext:
            return tokens
        return preprocess_tokens(tokens)
