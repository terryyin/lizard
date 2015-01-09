import unittest
from lizard import CLikeReader, CLikeReader
from mock import Mock
from .testHelpers import get_cpp_fileinfo, get_cpp_function_list

class Test_C_Token_extension(unittest.TestCase):

    def test_connecting_marcro(self):
        extended = CLikeReader(None).preprocess(("a##b c", ))
        #tbd

class Test_c_cpp_lizard(unittest.TestCase):

    def test_empty(self):
        result = get_cpp_function_list("")
        self.assertEqual(0, len(result))

    def test_no_function(self):
        result = get_cpp_function_list("#include <stdio.h>\n")
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_cpp_function_list("int fun(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)

    def test_two_function(self):
        result = get_cpp_function_list("int fun(){}\nint fun1(){}\n")
        self.assertEqual(2, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun1", result[1].name)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(1, result[0].end_line)
        self.assertEqual(2, result[1].start_line)
        self.assertEqual(2, result[1].end_line)

    def test_function_with_content(self):
        result = get_cpp_function_list("int fun(xx oo){int a; a= call(p1,p2);}")
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( xx oo )", result[0].long_name)

    def test_old_style_c_function(self):
        result = get_cpp_function_list("""int fun(param) int praram; {}""")
        self.assertEqual(1, len(result))

    def test_not_old_style_c_function(self):
        result = get_cpp_function_list("m()"*1500+ "a(){}")
        self.assertEqual(1, len(result))

    def test_complicated_c_function(self):
        result = get_cpp_function_list("""int f(int(*)()){}""")
        self.assertEqual('f', result[0].name)

    def test_function_dec_with_throw(self):
        result = get_cpp_function_list("""int fun() throw();void foo(){}""")
        self.assertEqual(1, len(result))

    def test_function_dec_followed_with_one_word_is_ok(self):
        result = get_cpp_function_list("""int fun() no_throw {}""")
        self.assertEqual(1, len(result))

    def test_function_declaration_is_not_counted(self):
        result = get_cpp_function_list("""int fun();class A{};""")
        self.assertEqual(0, len(result))

    def test_old_style_c_function_has_semicolon(self):
        result = get_cpp_function_list("""{(void*)a}{}""")
        self.assertEqual(0, len(result))

    def test_typedef_is_not_old_style_c_function(self):
        result = get_cpp_function_list('''typedef T() nT; foo(){}''')
        self.assertEqual("foo", result[0].name)

    def test_stupid_macro_before_function(self):
        result = get_cpp_function_list('''T() foo(){}''')
        self.assertEqual("foo", result[0].name)

    def test_only_word_can_be_function_name(self):
        result = get_cpp_function_list("""[(){}""")
        self.assertEqual(0, len(result))

    def test_double_slash_within_string(self):
        result = get_cpp_function_list("""int fun(){char *a="\\\\";}""")
        self.assertEqual(1, len(result))
    
    def test_function_with_no_param(self):
        result = get_cpp_function_list("int fun(){}")
        self.assertEqual(0, result[0].parameter_count)
    
    def test_function_with_1_param(self):
        result = get_cpp_function_list("int fun(aa * bb){}")
        self.assertEqual(1, result[0].parameter_count)
    
    def test_function_with_param(self):
        result = get_cpp_function_list("int fun(aa * bb, cc dd){}")
        self.assertEqual(2, result[0].parameter_count)
    
    def test_function_with_strang_param(self):
        result = get_cpp_function_list("int fun(aa<mm, nn> bb){}")
        self.assertEqual(1, result[0].parameter_count)
    
    def test_one_function_with_namespace(self):
        result = get_cpp_function_list("int abc::fun(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun( )", result[0].long_name)
    
    def test_one_function_with_const(self):
        result = get_cpp_function_list("int abc::fun()const{}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun( ) const", result[0].long_name)

    def test_one_function_in_class(self):
        result = get_cpp_function_list("class c {~c(){}}; int d(){}")
        self.assertEqual(2, len(result))
        self.assertEqual("c", result[0].name)
        self.assertEqual("d", result[1].name)

    def test_template_as_reference(self):
        result = get_cpp_function_list("abc::def(a<b>& c){}")
        self.assertEqual(1, len(result))

    def test_less_then_is_not_template(self):
        result = get_cpp_function_list("def(<); foo(){}")
        self.assertEqual(1, len(result))

    def test_template_with_pointer(self):
        result = get_cpp_function_list("abc::def (a<b*> c){}")
        self.assertEqual(1, len(result))

    def test_nested_template(self):
        result = get_cpp_function_list("abc::def (a<b<c>> c){}")
        self.assertEqual(1, len(result))

    def test_template_with_reference(self):
        result = get_cpp_function_list("void fun(t<int &>b){} ")
        self.assertEqual(1, len(result))

    def test_template_with_reference_as_reference(self):
        result = get_cpp_function_list("void fun(t<const int&>&b){} ")
        self.assertEqual(1, len(result))

    def test_template_as_part_of_function_name(self):
        result = get_cpp_function_list("void fun<a,b<c>>(){} ")
        self.assertEqual('fun<a,b<c>>', result[0].name)

    def test_operator_overloading(self):
        result = get_cpp_function_list("bool operator +=(int b){}")
        self.assertEqual("operator +=", result[0].name)

    def test_operator_with_complicated_name(self):
        result = get_cpp_function_list("operator MyStruct&(){}")
        self.assertEqual("operator MyStruct &", result[0].name)

    def test_operator_overloading_with_namespace(self):
        result = get_cpp_function_list("bool TC::operator !(int b){}")
        self.assertEqual(1, len(result))
        self.assertEqual("TC::operator !", result[0].name)

    def test_function_operator(self):
        result = get_cpp_function_list("bool TC::operator ()(int b){}")
        self.assertEqual(1, len(result))
        self.assertEqual("TC::operator ( )", result[0].name)

    def test_constructor_initialization_list(self):
        result = get_cpp_function_list('''A::A():a(1){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("A::A", result[0].name)

    def test_brakets_before_function(self):
        result = get_cpp_function_list('''()''')
        self.assertEqual(0, len(result))

    def test_function_that_returns_function_pointers(self):
        result = get_cpp_function_list('''int (*fun())(){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("int( * fun ( ) )", result[0].name)

    def test_underscore(self):
        from lizard import CodeReader
        generate_tokens = CodeReader.generate_tokens
        result = get_cpp_function_list(''' a() _() { }''')
        self.assertEqual(1, len(result))

class Test_Preprocessing(unittest.TestCase):

    def test_content_macro_should_be_ignored(self):
        result = get_cpp_function_list(r'''
                    #define MTP_CHEC                    \
                       int foo () {                     \
                        }
               ''')
        self.assertEqual(0, len(result))


    def test_preprocessors_should_be_ignored_outside_function_implementation(self):
        result = get_cpp_function_list('''
                      #ifdef MAGIC
                      #endif
                      void foo()
                      {}
                    ''')
        self.assertEqual(1, len(result))

    def test_preprocessor_is_not_function(self):
        result = get_cpp_function_list('''
                #ifdef A
                #elif (defined E)
                #endif
                ''')
        self.assertEqual(0, len(result))


class Test_Big(unittest.TestCase):

    def test_trouble(self):
        code = "foo<y () >> 5> r;"
        result = get_cpp_function_list(code)
        self.assertEqual(0, len(result))

    def test_namespace_is_not_function(self):
        code = "namespace a b(){}"
        result = get_cpp_function_list(code)
        self.assertEqual(0, len(result))


