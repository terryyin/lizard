'''
This is an extension of lizard, that counts the amount of bool
'''


class LizardExtension(object):  # pylint: disable=R0903

    def __init__(self):
        self.total_bool = 0
        self.total_token = 0

    def __call__(self, tokens, reader):
        reader.context.fileinfo.bool_count = 0
        for token in tokens:
            if token.lower() == "bool":
                reader.context.fileinfo.bool_count += 1
            yield token

    def cross_file_process(self, fileinfos):
        '''
        Combine the statistics from each file.
        '''
        for fileinfo in fileinfos:
            if hasattr(fileinfo, "bool_count"):
                self.total_bool += fileinfo.bool_count
            self.total_token += fileinfo.token_count
            yield fileinfo

    def print_result(self):
        if self.total_token == 0:
            self.total_token = 1
        print("Total non-comment token:", self.total_token)
        print("Total bool count:", self.total_bool)
        print("rate %:", (self.total_bool * 100.0) / self.total_token)
