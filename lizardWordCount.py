class LizardExtension(object):
    
    ignoreList = ('(',')','{','}',';',',', '\n')
    
    def __init__(self):
        self.result1 = {}
    
    def extend_tokens(self, tokens):
        self.result = {}
        for t in tokens:
            token = t[0].lower()
            if token not in self.ignoreList:
                self.result[token] = self.result.get(token, 0) + 1
            yield t
    
    def reduce(self, statistics):
        for k, v in statistics.wordCount.items():
            self.result1[k] = self.result1.get(k, 0) + v
            
    def print_result(self):
        for k in sorted(self.result1, key=self.result1.get):
            print(k, "\t", self.result1[k])