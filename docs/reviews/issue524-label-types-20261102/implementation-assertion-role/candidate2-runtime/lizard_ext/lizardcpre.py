'''
This is an extension of lizard,
It helps to deal with C code with preprocessors that
is hard to parse. It works by always ignoring the code
between #else and #end.
'''


class LizardExtension(object):  # pylint: disable=R0903

    ordering_index = 0

    def __call__(self, tokens, reader):
        def preprocess_tokens(tokens):
            else_count = 0
            if_stack = []
            for token in tokens:
                if token.startswith("#"):
                    if_stack.append(token)
                    else_count += token.count("#else")
                    if token.startswith("#endif"):
                        while if_stack:
                            last = if_stack.pop()
                            else_count -= last.count("#else")
                            if last.startswith("#if"):
                                break
                    for _ in range(token.count('\n')):
                        yield '\n'
                elif else_count:
                    for _ in range(token.count('\n')):
                        yield '\n'
                elif not (if_stack and if_stack[-1].startswith("#elif")):
                    yield token

        if "c" not in reader.ext:
            return tokens
        return preprocess_tokens(tokens)
