'''
This is an extension of lizard, that counts the amount of public functions in a file. Only works for c# code as of now.
'''

import re


class LizardExtension(object):  # pylint: disable=R0903

    pattern = r'public\s+(?:static\s+)?\w+\s+\w+\s*\(.*\)\s*{'

    def __init__(self):
        self.total_public_method_count = 0

    def __call__(self, tokens, reader):
        def count_public_methods(code):
            pattern_public_methods = r'\bpublic\s+(?:static\s+)?\w+\s+\w+\s*\([^)]*\)\s*'
            pattern_public_constructors = r'\bpublic\s+\w+\s*\([^)]*\)\s*'

            # Use re.findall to find all matches of the pattern in the code
            return (len(re.findall(pattern_public_methods, code))
                                       + len(re.findall(pattern_public_constructors, code)))

        reader.context.fileinfo.public_count_functions = 0
        tokens_code = []
        for token in tokens:
            tokens_code.append(token)
            yield token

        code = " ".join(tokens_code)
        reader.context.fileinfo.public_count_functions = count_public_methods(code)
        tokens_code.clear()

    def cross_file_process(self, fileinfos):
        '''
        Combine the statistics from each file.
        '''
        for fileinfo in fileinfos:
            if hasattr(fileinfo, "public_methods_count"):
                self.total_public_method_count += fileinfo.public_count_functions
            yield fileinfo

