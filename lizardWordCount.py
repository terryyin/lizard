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
        self.result1 = {}
    
    def extend_tokens(self, tokens):
        self.result = {}
        for token in tokens:
            if token not in self.ignoreList and token[0] not in ('"', "'", '#'):
                self.result[token] = self.result.get(token, 0) + 1
            yield token
    
    def reduce(self, statistics):
        for k, v in statistics.wordCount.items():
            self.result1[k] = self.result1.get(k, 0) + v
            
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
            for k in sorted(self.result1, key=self.result1.get, reverse = True)[:400]:
                f.write(' ' * 40 + '["%s", %d],\n' % (k.replace('"', '\\\"').replace("'", "\\\\'").replace("\\", "\\\\"), self.result1[k]))
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
                                                                                        

