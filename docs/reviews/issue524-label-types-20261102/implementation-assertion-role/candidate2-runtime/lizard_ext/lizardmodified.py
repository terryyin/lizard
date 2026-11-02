'''
This is an extension of lizard,
It lets lizard to use modified cyclomatic complexity number,
where the whole switch/case will be counted as 1.
'''


class LizardExtension(object):  # pylint: disable=R0903
    """
    Modified CCN extension: counts an entire switch/case (or Python match/case)
    as 1 complexity point instead of 1 per arm.
    Adds +1 for 'switch'/'match' (the block opener), subtracts -1 for each arm.
    """

    def __call__(self, tokens, reader):
        for token in tokens:
            if token in ('switch', 'match') and self._is_block_keyword(token, reader):
                reader.context.add_condition()
                if hasattr(reader.context, "add_nd_condition"):
                    reader.context.add_nd_condition()
            elif token == 'case' and self._is_case_keyword(reader):
                reader.context.add_condition(-1)
                if hasattr(reader.context, "add_nd_condition"):
                    reader.context.add_nd_condition(-1)
            yield token

    @staticmethod
    def _is_block_keyword(token, reader):
        """True when this token opens a switch-like block that counts as 1."""
        if token == 'switch':
            return True
        # 'match' is a soft keyword in Python; the reader sets _keyword_match.
        return getattr(reader, '_keyword_match', False)

    @staticmethod
    def _is_case_keyword(reader):
        """True when condition_counter or the reader has already counted this 'case'."""
        # Formal keyword languages (C, C++, Java, C#): condition_counter added +1.
        if 'case' in reader.conditions:
            return True
        # Python soft keyword: process_token will add +1 via _keyword_case.
        return getattr(reader, '_keyword_case', False)
