class LizardExtension(object):

    ignoreList = set(('(',')','{','}',';',',', '\n',
                '~',
                'static_cast',
                '&&',
                '#pragma',
                '!',
                'virtual',
                '++',
                'operator',
                '-',
                'private',
                'else',
                '+',
                '!=',
                '?',
                '/',
                ">=",
                "<=",
                "|=",
                "&=",
                "-=",
                "/=",
                "*=",
                'static',
                'inline',
                ']',
                '==',
                '+=',
                '[',
                '|',
                '||',
                'public',
                'struct',
                'typedef',
                'class',
                '<<',
                '#endif',
                '#if',
                'if',
                'for',
                'case',
                'break',
                'namespace',
                ':',
                '->',
                'return',
                'void',
                '*',
                '#include',
                '=',
                'const',
                '<',
                '>',
                '&',
                '\\',
                "\\\\\\",
                '.',
                '::',

            ))

    def __init__(self):
        self.result = {}

    @staticmethod
    def extend_tokens(tokens, context):
        '''
        The function will be used in multiple threading tasks.
        So don't store any data with an extension object.
        '''
        context.fileinfo.wordCount = result = {}
        for token in tokens:
            if token not in LizardExtension.ignoreList and token[0] not in ('"', "'", '#'):
                result[token] = result.get(token, 0) + 1
            yield token

    def reduce(self, fileinfo):
        '''
        Combine the statistics from each file.
        Because the statistics came from multiple thread tasks. This function needs
        to be called to collect the combined result.
        '''
        for k, v in fileinfo.wordCount.items():
            self.result[k] = self.result.get(k, 0) + v

    def print_result(self):
        with open('codecloud.html', 'w') as f:
            f.write('''
            <html>
                <head>
                    <script type="text/javascript" src="https://raw2.github.com/terryyin/tagcloud.js/master/tagcloud.js"></script> 
                    <script type="application/javascript">
                        function draw() {
                            var canvas = document.getElementById("canvas");
                                if (canvas.getContext) {
                                    var ctx = canvas.getContext("2d");
                                    var tagCloud = new TagCloud(canvas.width, canvas.height, ctx);
                                    tagCloud.render([''')
            for k in sorted(self.result, key=self.result.get, reverse = True)[:400]:
                f.write(' ' * 40 + '["%s", %d],\n' % (k.replace('"', '\\\"').replace("'", "\\\\'").replace("\\", "\\\\"), self.result[k]))
            f.write('''
                                    ]);
                                }
                        }
                    </script>
                </head>
                <body onload="draw();">
                    <canvas id="canvas" width="700" height="700"></canvas>
                </body>
            </html>''')
                                                                                        

