import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import get_reader_for, ObjCReader, CLikeReader


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

    def test_one_objc_function_with_two_param(self):
        result = self.create_objc_lizard("- (BOOL)scanJSONObject:(id *)outObject error:(NSError **)outError {}")
        self.assertEqual("scanJSONObject: error:", result[0].name)
        self.assertEqual("scanJSONObject:( id * ) error:( NSError ** )", result[0].long_name)

    def test_one_objc_function_with_three_param(self):
        result = self.create_objc_lizard("- (id)initWithRequest:(NSURLRequest *)request delegate:(id <NSURLConnectionDelegate>)delegate startImmediately:(BOOL)startImmediately{}")
        self.assertEqual("initWithRequest: delegate: startImmediately:", result[0].name)
        self.assertEqual("initWithRequest:( NSURLRequest * ) delegate:( id < NSURLConnectionDelegate > ) startImmediately:( BOOL )", result[0].long_name)

    def test_typedef(self):
        code = """
                typedef void(^alertActionHandler)(void);
                @implementation TestLizard
                - (void)method1 { }
                - (void)method2 { }
                @end
            """
        result = self.create_objc_lizard(code)
        self.assertEqual(2, len(result))
        self.assertEqual("method1", result[0].name)

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


class TestObjCLanguage(unittest.TestCase):
    """Tests for the GitHub issue #428 fix - .mm file extension mapping"""
    
    def setUp(self):
        self.analyzer = FileAnalyzer(get_extensions([ObjCReader]))

    def test_mm_extension_maps_to_objc_reader(self):
        """Test that .mm files are correctly mapped to ObjCReader, not CLikeReader"""
        self.assertEqual(ObjCReader, get_reader_for("test.mm"))
        self.assertNotEqual(CLikeReader, get_reader_for("test.mm"))

    def test_m_extension_maps_to_objc_reader(self):
        """Test that .m files are correctly mapped to ObjCReader"""
        self.assertEqual(ObjCReader, get_reader_for("test.m"))

    def test_objective_c_function_parsing(self):
        """Test that Objective-C functions are correctly parsed"""
        code = '''
        - (BOOL)application:(UIApplication *)application willFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
            return YES;
        }
        
        - (void)applicationDidBecomeActive:(UIApplication *)application {
            // Do something
        }
        '''
        result = analyze_file.analyze_source_code("test.mm", code)
        self.assertEqual(2, len(result.function_list))
        self.assertEqual("application: willFinishLaunchingWithOptions:", result.function_list[0].name)
        self.assertEqual("applicationDidBecomeActive:", result.function_list[1].name)

    def test_objective_cpp_function_parsing(self):
        """Test that Objective-C++ functions are correctly parsed in .mm files"""
        code = '''
        - (BOOL)application:(UIApplication *)application willFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
            std::string test = "hello";
            return YES;
        }
        '''
        result = analyze_file.analyze_source_code("test.mm", code)
        self.assertEqual(1, len(result.function_list))
        self.assertEqual("application: willFinishLaunchingWithOptions:", result.function_list[0].name)
        self.assertNotEqual("if", result.function_list[0].name)  # Should not show "if" as function name


