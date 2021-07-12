import unittest
from lizard import analyze_file


def get_java_fileinfo(source_code):
    return analyze_file.analyze_source_code("a.java", source_code)


def get_java_function_list(source_code):
    return get_java_fileinfo(source_code).function_list


class TestJava(unittest.TestCase):

    def test_function_with_throws(self):
        result = get_java_function_list("void fun() throws e1, e2{}")
        self.assertEqual(1, len(result))

    def test_function_with_decorator(self):
        result = get_java_function_list("@abc() void fun() throws e1, e2{}")
        self.assertEqual(1, len(result))

    def test_class_with_decorator(self):
        result = get_java_function_list("@abc() class funxx{ }")
        self.assertEqual(0, len(result))

    def test_class_with_decorator_that_has_namespace(self):
        result = get_java_function_list("@a.b() class funxx{ }")
        self.assertEqual(0, len(result))

    def test_class_name_with_extends(self):
        result = get_java_function_list("class A extends B { void f(){}}")
        self.assertEqual('A::f', result[0].name)

    def test_class_name_with_interface(self):
        result = get_java_function_list("class A implements B { void f(){}}")
        self.assertEqual('A::f', result[0].name)

    def test_operator_as_an_overloaded_identifier(self):
        """it turns out you can overload the operator keyword"""
        result = get_java_function_list("""
            package operator; class A { void f(){}}
        """)
        self.assertEqual("A::f", result[0].name)

    def test_abstract_function_without_body_following_method(self):
        result = get_java_function_list("abstract void fun(); void fun1(){}")
        self.assertEqual("fun1", result[0].name)
        self.assertEqual(1, len(result))

    def test_abstract_function_without_body_with_throws_following_method(self):
        result = get_java_function_list("abstract void fun() throws e; void fun2(){}")
        self.assertEqual("fun2", result[0].name)
        self.assertEqual(1, len(result))

    def test_generic_type_with_extends(self):
        result = get_java_function_list("class B<T extends C> {void fun(T t) {}}")
         # actual "B<T::fun"
        self.assertEqual("B::fun", result[0].name)


    def test_generic_type_with_question_mark(self):
        result = get_java_function_list("void A(){ List<? extends x> list;}}")
        self.assertEqual(1, len(result))
        self.assertEqual("A", result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)
