'''
This is an extension of lizard. It add a list of language keywords
that adding the complexity and the line numbers of the keywords appear.
'''


class LizardExtension(object):  # pylint: disable=R0903
    """
    Complex tags extension: records all complexity-adding keywords and their line numbers.
    Uses reader.conditions (combined set of all condition types) to track all
    complexity contributors: control flow, logical operators, case labels, and ternary.
    """

    # pylint: disable=W0221
    def __call__(self, tokens, reader):
        context = reader.context
        # Use combined conditions set - intentionally includes all types
        conditions = reader.conditions
        for token in tokens:
            yield token
            if not hasattr(context.current_function, 'complex_tags'):
                context.current_function.complex_tags = []
            if token in conditions:
                context.current_function.complex_tags.append(
                        [token, context.current_line])
