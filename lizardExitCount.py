'''
This is an extension of lizard, that counts the 'exit points' in every function.
'''
class LizardExtension(object):

    def extend_tokens(self, tokens, context):
        first_return = False
        for token in tokens:
            if not hasattr(context.current_function, "exit_count"):
                context.current_function.exit_count = 1
                first_return = True
            if token == "return":
                if first_return:
                    first_return = False
                else:
                    context.current_function.exit_count += 1
            yield token
