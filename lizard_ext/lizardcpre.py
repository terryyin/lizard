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
                    if token.startswith("#if"):
                        if_stack.append(token)
                    elif token.startswith("#el"):
                        else_count += 1
                        if_stack.append(token)
                    elif token.startswith("#endif"):
                        if if_stack and if_stack.pop().startswith("#el"):
                            else_count -= 1
                            if if_stack:
                                if_stack.pop()
                    continue
                if not else_count:
                    yield token

        if "c" not in reader.ext:
            return tokens
        else:
            return preprocess_tokens(tokens)
