class CFunctionBuilder(object):
    def __init__(self):
        self.code = ''

    def build(self):
        code = self.code
        self.code = ''
        return code

    def empty_function(self):
        self.code += ''' void func0() {
            }
        '''
        return self

    def five_line_function(self, name='func6'):
        self.code += ''' string %s(int param, T t) {
                for (int i; i < 100; i++)
                    result += i + i;
                return i * 5 + 1015;
            }
        '''%(name)
        return self

    def six_line_function(self, name='func6', variable_name='result', number_value="10", rvalue='"abc"', operator="+"):
        self.code += ''' void %s(int param) {
                int %s, i = 0;
                for (; i < %s; i++) {
                     print(%s);%s += i %s i;
                }
            }
        '''%(name, variable_name, number_value, rvalue, variable_name, operator)
        return self

    def part_of_six_line_function(self, name='func6'):
        self.code += ''' void %s(int param) {
                int result, i = 0;
                for (; i < 10; i++) {
                     print("abc");result += i + i;
                    then = "I do something else";
                }
            }
        '''%(name)
        return self

    def different_six_line_function(self, name='func6'):
        self.code += ''' int %s(int param, int c) {
                if (abc == def) {
                    while(param) c=c+1;
                }
                return result;
            }
        '''%(name)
        return self
