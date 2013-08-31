import unittest
from hfcca import  FileAnalyzer, CTokenTranslator, generate_tokens

def create_cpp_hfcca(source_code):
    return FileAnalyzer().analyze_source_code_with_parser("", CTokenTranslator(), generate_tokens(source_code))

class Test_c_cpp_hfcca(unittest.TestCase):
    def test_empty(self):
        result = create_cpp_hfcca("")
        self.assertEqual(0, len(result))
        
    def test_no_function(self):
        result = create_cpp_hfcca("#include <stdio.h>\n")
        self.assertEqual(0, len(result))
    
    def test_one_function(self):
        result = create_cpp_hfcca("int fun(){}")
        self.assertEqual(1, len(result))
        self.assertTrue("fun" in result)
        self.assertEqual(1, result[0].cyclomatic_complexity)
    
    def test_two_function(self):
        result = create_cpp_hfcca("int fun(){}\nint fun1(){}\n")
        self.assertEqual(2, result.nloc)
        self.assertEqual(2, len(result))
        self.assertTrue("fun" in result)
        self.assertTrue("fun1" in result)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(2, result[1].start_line)
    
    def test_function_with_content(self):
        result = create_cpp_hfcca("int fun(xx oo){int a; a= call(p1,p2);}")
        self.assertEqual(1, len(result))
        self.assertTrue("fun" in result)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual("fun( xx oo )", result[0].long_name())
    
    def test_one_function_with_content(self):
        result = create_cpp_hfcca("int fun(){if(a){xx;}}")
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(1, result[0].nloc)

    def test_nloc_of_empty_function(self):
        result = create_cpp_hfcca("int fun(){}")
        self.assertEqual(0, result[0].nloc)
    
    def test_nloc(self):
        result = create_cpp_hfcca("int fun(){\n\n\n}")
        self.assertEqual(0, result[0].nloc)
    
    def test_nloc2(self):
        result = create_cpp_hfcca("int fun(){aa();\n\n\n\nbb();\n\n\n}")
        self.assertEqual(2, result[0].nloc)
    
    def test_one_function_with_question_mark(self):
        result = create_cpp_hfcca("int fun(){return (a)?b:c;}")
        self.assertEqual(2, result[0].cyclomatic_complexity)
    
    def test_one_function_with_forever_loop(self):
        result = create_cpp_hfcca("int fun(){for(;;){dosomething();}}")
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)
    
    def test_one_function_with_and(self):
        result = create_cpp_hfcca("int fun(){if(a&&b){xx;}}")
        self.assertEqual(3, result[0].cyclomatic_complexity)
    
    def test_one_function_with_else_if(self):
        result = create_cpp_hfcca("int fun(){if(a)b;else if (c) d;}")
        self.assertEqual(3, result[0].cyclomatic_complexity)
    
    def test_double_slash_within_string(self):
        result = create_cpp_hfcca("""int fun(){char *a="\\\\";}""")
        self.assertEqual(1, result[0].cyclomatic_complexity)
    
    def test_example_macro(self):
        result = create_cpp_hfcca(example_macro)
        self.assertEqual(0, len(result))
    
    def test_function_with_no_param(self):
        result = create_cpp_hfcca("int fun(){}")
        self.assertEqual(0, result[0].parameter_count)
    
    def test_function_with_1_param(self):
        result = create_cpp_hfcca("int fun(aa * bb){}")
        self.assertEqual(1, result[0].parameter_count)
    
    def test_function_with_param(self):
        result = create_cpp_hfcca("int fun(aa * bb, cc dd){}")
        self.assertEqual(2, result[0].parameter_count)
    
    def test_function_with_strang_param(self):
        result = create_cpp_hfcca("int fun(aa<mm, nn> bb){}")
        self.assertEqual(1, result[0].parameter_count)
    
    def test_one_function1(self):
        result = create_cpp_hfcca("int abc::fun(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun( )", result[0].long_name())
        self.assertEqual(1, result[0].cyclomatic_complexity)
    
    def test_one_function_with_const(self):
        result = create_cpp_hfcca("int abc::fun()const{}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun( ) const", result[0].long_name())
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_function_in_class(self):
        result = create_cpp_hfcca("class c {~c(){}}; int d(){}")
        self.assertEqual(2, len(result))
        self.assertTrue("c" in result)
        self.assertTrue("d" in result)
        
    def test_template_as_reference(self):
        result = create_cpp_hfcca("abc::def(a<b>& c){}")
        self.assertEqual(1, len(result))

    def test_template_with_pointer(self):
        result = create_cpp_hfcca("abc::def(a<b*> c){}")
        self.assertEqual(1, len(result))
        
    def test_template_with_reference(self):
        result = create_cpp_hfcca("void fun(t<int &>b){} ")
        self.assertEqual(1, len(result))
        
    def test_template_with_reference_as_reference(self):
        result = create_cpp_hfcca("void fun(t<const int&>&b){} ")
        self.assertEqual(1, len(result))
       
    def test_operator_overloading(self):
        result = create_cpp_hfcca("bool operator +=(int b){}")
        self.assertEqual(1, len(result))
        self.assertEqual("operator +=", result[0].name)
                
    def test_operator_overloading_with_namespace(self):
        result = create_cpp_hfcca("bool TC::operator !(int b){}")
        self.assertEqual(1, len(result))
        self.assertEqual("TC::operator !", result[0].name)
                
    def test_function_operator(self):
        result = create_cpp_hfcca("bool TC::operator ()(int b){}")
        self.assertEqual(1, len(result))
        self.assertEqual("TC::operator ( )", result[0].name)

class Test_C_Function_Token_Count(unittest.TestCase):

    def test_one_function_with_no_token(self):
        result = create_cpp_hfcca("int fun(){}")
        self.assertEqual(0, result[0].token_count)

    def test_one_function_with_one_token(self):
        result = create_cpp_hfcca("int fun(){;}")
        self.assertEqual(1, result[0].token_count)

    def test_one_function_with_content(self):
        result = create_cpp_hfcca("int fun(){if(a){xx;}}")
        self.assertEqual(8, result[0].token_count)

    def test_one_function_with_comments_only(self):
        result = create_cpp_hfcca("int fun(){/**/}")
        self.assertEqual(0, result[0].token_count)
        
class Test_Preprocessing(unittest.TestCase):

    def test_braces_in_harsh_else(self):
        result = create_cpp_hfcca('''int main(){
                                        #ifndef NORBUS
                                        {
                                        #else
                                        {
                                        #endif
                                        }
                                    } void fun(){}''')
        self.assertEqual(2, len(result))

example_macro = r'''
#define MTP_CHECK                                                             \
   if (mea_data->which_data != MTP_WHICH_DATA_T_NONE_C) {                     \
   phys_address_t np;                                                         \
   }
'''
