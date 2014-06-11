'''
This is an extension of lizard, that counts the complexity outside functions
'''


class LizardExtension(object):  # pylint: disable=R0903

    def __call__(self, tokens, reader):
        for token in tokens:
            yield token
        # adding the global pseudo function to the result
        reader.context.end_of_function()
