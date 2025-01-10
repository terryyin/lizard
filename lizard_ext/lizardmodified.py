'''
This is an extension of lizard,
It lets lizard to use modified cyclomatic complexity number,
where the whole switch/case will be counted as 1.
'''


from tokenize import Token


class LizardExtension(object):  # pylint: disable=R0903

    def __call__(self, tokens, reader):
        switch_type = False
        case_type = False
        for token in tokens:
            if token == 'switch' and switch_type == False and case_type == False:
                switch_type = True
            if token == 'case' and switch_type == False and case_type == False:
                case_type = True
            if (switch_type and token == 'switch') or (case_type and token == 'case'):
                reader.context.add_condition()
                if hasattr(reader.context, "add_nd_condition"):
                    reader.context.add_nd_condition()
            elif (switch_type and token == 'case') or (case_type and token == 'when'):
                reader.context.add_condition(-1)
                if hasattr(reader.context, "add_nd_condition"):
                    reader.context.add_nd_condition(-1)
            yield token
