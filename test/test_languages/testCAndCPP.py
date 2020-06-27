import unittest
from lizard_languages import CLikeReader
from ..testHelpers import get_cpp_fileinfo, get_cpp_function_list

class Test_C_Token_extension(unittest.TestCase):

    def test_connecting_macro(self):
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

    def test_two_simplest_function(self):
        result = get_cpp_function_list("f(){}g(){}")
        self.assertEqual(2, len(result))
        self.assertEqual("f", result[0].name)
        self.assertEqual("g", result[1].name)

    def test_function_with_content(self):
        result = get_cpp_function_list("int fun(xx oo){int a; a= call(p1,p2);}")
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( xx oo)", result[0].long_name)

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

    def test_function_dec_with_noexcept(self):
        result = get_cpp_function_list("int fun() noexcept(true);void foo(){}")
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
        result = get_cpp_function_list("int fun(aa bb){}")
        self.assertEqual(1, result[0].parameter_count)
        self.assertEqual(["bb"], result[0].parameters)

    def test_function_with_1_ref_param(self):
        result = get_cpp_function_list("int fun(aa * bb){}")
        self.assertEqual(1, result[0].parameter_count)
        self.assertEqual(["bb"], result[0].parameters)

    def test_function_with_param(self):
        result = get_cpp_function_list("int fun(aa * bb, cc dd){}")
        self.assertEqual(2, result[0].parameter_count)

    def test_function_with_strang_param(self):
        result = get_cpp_function_list("int fun(aa<mm, nn> bb){}")
        self.assertEqual(1, result[0].parameter_count)
        self.assertEqual("fun( aa<mm,nn> bb)", result[0].long_name)

    def test_function_with_strang_param2(self):
        result = get_cpp_function_list("int fun(aa<x<mm,(x, y)>, nn> bb, (cc)<xx, oo> d){}")
        self.assertEqual(2, result[0].parameter_count)

    def test_one_function_with_namespace(self):
        result = get_cpp_function_list("int abc::fun(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun()", result[0].long_name)

    def test_one_function_with_const(self):
        result = get_cpp_function_list("int abc::fun()const{}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        self.assertEqual("abc::fun() const", result[0].long_name)

    def test_one_function_with_throw(self):
        result = get_cpp_function_list("""int fun() throw() {}""")
        self.assertEqual(1, len(result))
        self.assertEqual('fun', result[0].name)
        result = get_cpp_function_list("""int fun() throw(Exception) {}""")
        self.assertEqual(1, len(result))
        self.assertEqual('fun', result[0].name)

    def test_one_function_with_noexcept(self):
        result = get_cpp_function_list("int abc::fun()noexcept{}")
        self.assertEqual(1, len(result))
        self.assertEqual("abc::fun", result[0].name)
        result = get_cpp_function_list("int fun() noexcept(true) {}")
        self.assertEqual(1, len(result))
        self.assertEqual('fun', result[0].name)
        result = get_cpp_function_list(
                "int fun() noexcept(noexcept(foo()) && noexcept(Bar())) {}")
        self.assertEqual(1, len(result))
        self.assertEqual('fun', result[0].name)

    def test_two_functions_in_class(self):
        result = get_cpp_function_list("class c {~c(){}}; int d(){}")
        self.assertEqual(2, len(result))
        self.assertEqual("c::~c", result[0].name)
        self.assertEqual("d", result[1].name)

    def test_one_macro_in_class(self):
        result = get_cpp_function_list("class c {M()}; int d(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("d", result[0].name)

    def test_pre_class(self):
        result = get_cpp_function_list("class c; int d(){}")
        self.assertEqual(1, len(result))
        self.assertEqual("d", result[0].name)

    def test_class_with_inheritance(self):
        result = get_cpp_function_list("class c final:public b {int f(){}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)

    def test_nested_class(self):
        result = get_cpp_function_list("class c {class d {int f(){}};};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::d::f", result[0].name)

    def test_template_class(self):
        result = get_cpp_function_list("template<typename T> class c {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<class T> class c {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<typename T> class c {"
                                       "void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("template<class T> class c {"
                                       "void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("template<class T, typename S>"
                                       "class c {void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("namespace ns { template<class T>"
                                       "class c {void f(T t) {}}; }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::c::f", result[0].name)

    def test_template_class_full_specialization(self):
        result = get_cpp_function_list("template<> class c<double> {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<> class c<double> {"
                                       "void f() {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c<double>::f", result[0].name)
        result = get_cpp_function_list("template<>"
                                       "class c<double, int> {void f() {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c<double,int>::f", result[0].name)
        result = get_cpp_function_list("namespace ns { template<>"
                                       "class c<double> {void f() {}}; }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::c<double>::f", result[0].name)

    def test_template_class_partial_specialization(self):
        result = get_cpp_function_list(
                "template<typename T> class c<int,T> {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<class T> class c<int,T> {};")
        self.assertEqual(0, len(result))
        result = get_cpp_function_list("template<typename T> class c<int,T> {"
                                       "void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c<int,T>::f", result[0].name)
        result = get_cpp_function_list("template<class T> class c<int,T> {"
                                       "void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c<int,T>::f", result[0].name)
        result = get_cpp_function_list("template<class T, typename S>"
                                       "class c<int,T,S> {void f(T t) {}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c<int,T,S>::f", result[0].name)
        result = get_cpp_function_list("namespace ns { template<class T>"
                                       "class c<int,T> {void f(T t) {}}; }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::c<int,T>::f", result[0].name)

    def test_template_function(self):
        result = get_cpp_function_list("template<typename T> void f(T t) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("f", result[0].name)
        result = get_cpp_function_list("template<class T> void f(T t) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("f", result[0].name)
        result = get_cpp_function_list("namespace ns {"
                                       "template<class T> void f(T t) {}}")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::f", result[0].name)

    def test_template_function_specialization(self):
        result = get_cpp_function_list("template<> void f<double>() {}")
        self.assertEqual(1, len(result))
        self.assertEqual("f<double>", result[0].name)
        result = get_cpp_function_list("namespace ns {"
                                       "template<> void f<double>() {}}")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::f<double>", result[0].name)

    def test_nested_template_function(self):
        result = get_cpp_function_list("template<typename T> class c { "
                                       "template<typename S> void f() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("template<class T> class c { "
                                       "template<class S> void f() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)
        result = get_cpp_function_list("namespace ns { "
                                       "template<class T> class c { "
                                       "template<class S> void f() {} }; }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::c::f", result[0].name)

    def test_templated_code_with_question_mark(self):
        result = get_cpp_function_list("void a(){Class<?>[];}")
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_class_as_an_attribute(self):
        result = get_cpp_function_list("void a(){{String.class}}")
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_1(self):
        result = get_cpp_function_list("class c {{}}")
        self.assertEqual(0, len(result))

    def test_bracket_that_is_not_a_namespace(self):
        result = get_cpp_function_list("class c { {};int f(){}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)

    def test_nested_class_middle(self):
        result = get_cpp_function_list("class c {class d {};int f(){}};")
        self.assertEqual(1, len(result))
        self.assertEqual("c::f", result[0].name)

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

    def test_double_nested_template(self):
        result = get_cpp_function_list("abc::def (a<b<c<d>>> c){}")
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

    def test_operator_overloading_shift(self):
        result = get_cpp_function_list("bool operator <<(int b){}")
        self.assertEqual("operator < <", result[0].name)

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

    def test_inline_operator(self):
        result = get_cpp_function_list("class A { bool operator ()(int b) {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::operator ( )", result[0].name)

    def test_namespace_alias(self):
        result = get_cpp_function_list(
                "namespace p;"
                "namespace real { bool foo() {} }")
        self.assertEqual(1, len(result))
        self.assertEqual("real::foo", result[0].name)

    def test_nested_unnamed_namespace(self):
        result = get_cpp_function_list(
                "namespace real { namespace { bool foo() {} } }")
        self.assertEqual(1, len(result))
        self.assertEqual("real::foo", result[0].name)

    def test_constructor_initialization_list(self):
        result = get_cpp_function_list('''A::A():a(1){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("A::A", result[0].name)

    def test_constructor_initialization_list_noexcept(self):
        result = get_cpp_function_list('''A::A()noexcept:a(1){}''')

    def test_constructor_initializer_list(self):
        result = get_cpp_function_list('''A::A():a({1}),value(true){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("A::A", result[0].name)

    def test_constructor_uniform_initialization(self):
        result = get_cpp_function_list('''A::A():a{1}{}''')
        self.assertEqual(1, len(result))
        self.assertEqual("A::A", result[0].name)

    def test_parentheses_before_function(self):
        result = get_cpp_function_list('''()''')
        self.assertEqual(0, len(result))

    def test_destructor_implementation(self):
        result = get_cpp_function_list('''A::~A(){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("A::~A", result[0].name)

    def test_function_that_returns_function_pointers(self):
        result = get_cpp_function_list('''int (*fun())(){}''')
        self.assertEqual(1, len(result))
        self.assertEqual("int( * fun())", result[0].name)

    def test_struct_in_return_type(self):
        result = get_cpp_function_list(''' struct a b() { a(1>2); }''')
        self.assertEqual(1, len(result))
        self.assertEqual("b", result[0].name)

    def test_function_name_class(self):
        result = get_cpp_function_list('''int class(){}''');
        self.assertEqual(1, len(result))

    def test_underscore(self):
        from lizard_languages.code_reader import CodeReader
        generate_tokens = CodeReader.generate_tokens
        result = get_cpp_function_list(''' a() _() { }''')
        self.assertEqual(1, len(result))

    def test_global_var_constructor(self):
        result = get_cpp_function_list('''std::string s("String");''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''std::string s = "String";''')
        self.assertEqual(0, len(result))

    def test_non_function_initializer_list(self):
        result = get_cpp_function_list('''v={}''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''v = {};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''std::vector<int> v = {1, 2, 3};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''v = {1, 2, 3};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''namespace n { v = {}; }''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''class n { int v = {0}; }''')
        self.assertEqual(0, len(result))

    def test_non_function_uniform_initialization(self):
        result = get_cpp_function_list('''std::vector<int> v{1, 2, 3};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''std::vector<int> v{};''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''namespace n { int v{0}; }''')
        self.assertEqual(0, len(result))
        result = get_cpp_function_list('''class n { int v{0}; }''')
        self.assertEqual(0, len(result))

    def test_struct_in_param(self):
        result = get_cpp_function_list('''int fun(struct a){}''')
        self.assertEqual(1, len(result))

    def test_trailing_return_type(self):
        """C++11 trailing return type for functions."""
        result = get_cpp_function_list("auto foo() -> void {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        result = get_cpp_function_list("auto foo(int a) -> decltype(a) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)

    def test_ref_qualifiers(self):
        """C++11 ref qualifiers for member functions."""
        result = get_cpp_function_list("struct A { void foo() & {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)
        result = get_cpp_function_list("struct A { void foo() const & {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)
        result = get_cpp_function_list("struct A { void foo() && {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)
        result = get_cpp_function_list("struct A { void foo() const && {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)

    def test_union_as_qualifier(self):
        """Union as namespace for functions."""
        result = get_cpp_function_list("union A { void foo() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)

class Test_cpp11_Attributes(unittest.TestCase):
    """C++11 extendable attributes can appear pretty much anywhere."""

    def test_namespace(self):
        result = get_cpp_function_list(
            "namespace [[visibility(hidden)]] ns { void foo() {} }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::foo", result[0].name)
        result = get_cpp_function_list(
            "namespace ns [[deprecated]] { void foo() {} }")
        self.assertEqual(1, len(result))
        self.assertEqual("ns::foo", result[0].name)

    def test_class(self):
        result = get_cpp_function_list(
            "struct [[alignas(8)]] A { void foo() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)
        result = get_cpp_function_list(
            "struct A [[deprecated]] { void foo() {} };")
        self.assertEqual(1, len(result))
        self.assertEqual("A::foo", result[0].name)

    def test_function(self):
        result = get_cpp_function_list("void foo() [[noreturn]] {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)

    def test_function_parameters(self):
        result = get_cpp_function_list("void foo(int a [[unused]]) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        result = get_cpp_function_list("void foo(int a [[unused]], int b) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        result = get_cpp_function_list("void foo(int b, int a [[unused]]) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)

    def test_function_return_type(self):
        result = get_cpp_function_list(
            "int [[warn_unused_result]] foo(int a) {}")
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)

    def test_control_structures(self):
        result = get_cpp_function_list(
            "int foo() { [[likely(true)]] if (a) return 1; else return 2; }")
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)
        result = get_cpp_function_list(
            """int foo() {
                 for [[omp::parallel()]] (int i{}; i < n; ++i)
                   sum += i; }""")
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)


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

    def test_body_with_function_like(self):
        '''in the following example 'xws' is a macro defined somewhere else'''
        result = get_cpp_function_list("""int a() { xws (a) if(){} }""")
        self.assertEqual(1, len(result))
        self.assertEqual('a', result[0].name)

    def test_body_with_macro_call_after_if(self):
        result = get_cpp_function_list("""int a() { if (a) b(){} }""")
        self.assertEqual(1, len(result))
        self.assertEqual('a', result[0].name)

    def test_body_with_macro_call_after_if_and_no_semicolon_before_the_closing_br(self):
        result = get_cpp_function_list("""int a() { if (a) b() } int c(){}""")
        self.assertEqual(2, len(result))
        self.assertEqual('c', result[1].name)

    def test_body_with_macro_call_after_if_and_no_semicolon_before_the_closing_br2(self):
        result = get_cpp_function_list("""int a() { if (a) if(x) b() } int c(){}""")
        self.assertEqual(2, len(result))
        self.assertEqual('c', result[1].name)

    def test_body_with_macro_and_class(self):
        result = get_cpp_function_list("""class A{int a() { if (a) if(x) b() } int c(){}}""")
        self.assertEqual(2, len(result))
        self.assertEqual('A::c', result[1].name)

    def test_body_with_function_like2(self):
        '''in the following example 'b' is a macro defined somewhere else'''
        result = get_cpp_function_list("""
void myFunction()
{
  IGNORE_FLAGS("w-maybe")
  if(2+2==4)
  END_IGNORE_FLAGS("w-maybe")
  {
    mySecondFunction()
  }
}

int mySecondFunction()
{
  return 2;
}
                """)
        self.assertEqual(2, len(result))
        self.assertEqual('mySecondFunction', result[1].name)


class Test_Big(unittest.TestCase):

    def test_trouble(self):
        code = "foo<y () >> 5> r;"
        result = get_cpp_function_list(code)
        self.assertEqual(0, len(result))

    def test_typedef(self):
        code = """
        typedef struct tagAAA
        {
        }AAA;

        int func_a(int size)
        {
            if(ccc && eee)
            {
                return 1;
            }
        }
        """
        result = get_cpp_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)



class Test_Dialects(unittest.TestCase):

    def test_cuda_kernel_launch(self):
        """Special triple < and > for Nvidia CUDA C/C++ code."""
        result = get_cpp_function_list('''void foo() {
                kernel <<< gridDim, blockDim, 0 >>> (d_data, height, width);
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("foo", result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        result = get_cpp_function_list('''void foo() {
                kernel <<< gridDim, blockDim, (bar ? 0 : 1) >>> (x, y, z);
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)
        result = get_cpp_function_list('''void foo() {
                kernel <<< gridDim, blockDim, 0 >>> (x, y, (bar ? w : z));
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)
