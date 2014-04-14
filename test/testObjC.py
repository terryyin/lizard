import unittest
from lizard import  analyze_file


class Test_objc_lizard(unittest.TestCase):

    def create_objc_lizard(self, source_code):
        return analyze_file.analyze_source_code("a.m", source_code).function_list

    def test_empty(self):
        result = self.create_objc_lizard("")
        self.assertEqual(0, len(result))

    def test_no_function(self):
        result = self.create_objc_lizard("#import <unistd.h>\n")
        self.assertEqual(0, len(result))

    def test_one_c_function(self):
        result = self.create_objc_lizard("int fun(int a, int b) {}")
        self.assertEqual("fun", result[0].name)

    def test_one_objc_function(self):
        result = self.create_objc_lizard("-(void) foo {}")
        self.assertEqual("foo", result[0].name)

    def test_one_objc_function_with_param(self):
        result = self.create_objc_lizard("-(void) replaceScene: (CCScene*) scene {}")
        self.assertEqual("replaceScene:", result[0].name)
        self.assertEqual("replaceScene:( CCScene * )", result[0].long_name)

    def test_one_objc_functio_nwith_two_param(self):
        result = self.create_objc_lizard("- (BOOL)scanJSONObject:(id *)outObject error:(NSError **)outError {}")
        self.assertEqual("scanJSONObject: error:", result[0].name)
        self.assertEqual("scanJSONObject:( id * ) error:( NSError ** )", result[0].long_name)

    def test_one_objc_function_with_three_param(self):
        result = self.create_objc_lizard("- (id)initWithRequest:(NSURLRequest *)request delegate:(id <NSURLConnectionDelegate>)delegate startImmediately:(BOOL)startImmediately{}")
        self.assertEqual("initWithRequest: delegate: startImmediately:", result[0].name)
        self.assertEqual("initWithRequest:( NSURLRequest * ) delegate:( id < NSURLConnectionDelegate > ) startImmediately:( BOOL )", result[0].long_name)

    def test_implementation(self):
        code = """
            @implementation classname(xx)
            + (return_type)classMethod
            {
                if (failure){

                     //wefailed

                 }
            }
            - (return_type)instanceMethod
            {
                // implementation
            }
            @end
            """
        result = self.create_objc_lizard(code)
        self.assertEqual(2, len(result))
        self.assertEqual("classMethod", result[0].name)


