'''
This is an extension of lizard,
It lets lizard to use modified cyclomatic complexity number,
where the whole switch/case will be counted as 1.
'''


class LizardExtension(object):  # pylint: disable=R0903
    """
    Modified CCN extension: counts entire switch/case as 1 complexity.
    Adds +1 for 'switch', subtracts -1 for each 'case'.
    Works with switch/case keywords (conceptually from reader.case_keywords).
    """

    def __call__(self, tokens, reader):
        for token in tokens:
            if token == 'switch':  # Add complexity for switch statement
                reader.context.add_condition()
                if hasattr(reader.context, "add_nd_condition"):
                    reader.context.add_nd_condition()
            elif token == 'case':  # Subtract complexity for each case
                reader.context.add_condition(-1)
                if hasattr(reader.context, "add_nd_condition"):
                    reader.context.add_nd_condition(-1)
            yield token
