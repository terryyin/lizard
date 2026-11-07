import unittest
from .java_helpers import get_java_function_list


class TestJava(unittest.TestCase):

    def test_my_code(self):
        code = """
public String[] funcA() {
    return properties.stream().toArray(String[]::new);
}

public String funcB() {
    return "something";
}
"""
        result = get_java_function_list(code)
        self.assertEqual(2, len(result))
        self.assertEqual('funcA()', result[0].long_name)
        self.assertEqual('funcB()', result[1].long_name)

    def test_function_with_throws(self):
        result = get_java_function_list("void fun() throws e1, e2{}")
        self.assertEqual(1, len(result))

    def test_anonymous_class_in_field_initializer(self):
        code = """
class T {
    private ThreadLocal<Long> startTime = new ThreadLocal<Long>() {
        @Override protected Long initialValue() { return 0L; }
    };
    void realMethod() { System.out.println("hi"); }
}
"""
        result = get_java_function_list(code)
        names = [f.name for f in result]
        # the field declaration must not be picked up as a method
        self.assertNotIn("T::ThreadLocal<Long>", names)
        # the anonymous class's real method should be counted
        self.assertIn("(anonymous)::initialValue", names)
        self.assertIn("T::realMethod", names)

    def test_generic_anonymous_class_in_method_body(self):
        code = """
class T {
    void m() {
        ThreadLocal<Long> x = new ThreadLocal<Long>() {
            protected Long initialValue() { return 0L; }
        };
    }
}
"""
        result = get_java_function_list(code)
        names = [f.name for f in result]
        self.assertIn("T::m", names)
        self.assertIn("(anonymous)::initialValue", names)

    def test_nested_generic_anonymous_class(self):
        code = """
class T {
    private Map<String, List<Long>> m = new HashMap<String, List<Long>>() {
        public int customSize() { if (isEmpty()) return 0; return 1; }
    };
    void realOuter() {}
}
"""
        result = get_java_function_list(code)
        names = [f.name for f in result]
        self.assertEqual(2, len(result))
        self.assertIn("(anonymous)::customSize", names)
        self.assertIn("T::realOuter", names)

    def test_wildcard_generic_anonymous_class(self):
        code = """
class T {
    void m() {
        Comparator<? super String> c = new Comparator<? super String>() {
            public int compare(String a, String b) { return 0; }
        };
    }
}
"""
        result = get_java_function_list(code)
        names = [f.name for f in result]
        self.assertIn("T::m", names)
        self.assertIn("(anonymous)::compare", names)

    def test_qualified_type_anonymous_class(self):
        code = """
class T {
    private java.util.Map<String, String> m = new java.util.HashMap<String, String>() {
        public int customSize() { return size(); }
    };
    void realOuter() {}
}
"""
        result = get_java_function_list(code)
        names = [f.name for f in result]
        self.assertEqual(2, len(result))
        self.assertNotIn("T::HashMap<String,String>", names)
        self.assertIn("(anonymous)::customSize", names)
        self.assertIn("T::realOuter", names)

    def test_function_with_decorator(self):
        result = get_java_function_list("@abc() void fun() throws e1, e2{}")
        self.assertEqual(1, len(result))

    def test_transactional_rollback_for_annotation(self):
        """@Transactional(rollbackFor = Exception.class) must not parse inner names as methods."""
        code = """
public class LizardTest {
    @Transactional(rollbackFor = Exception.class)
    public void test1() {
        List<String> list = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(list)) {
            list.add("test");
        }
        for (String str : list) {
            System.out.println(str);
        }
    }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("LizardTest::test1", result[0].name)
        self.assertEqual(3, result[0].cyclomatic_complexity)

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

    def test_many_question_marks_after_less_than_no_freeze(self):
        """Multiple ? after < must not cause catastrophic backtracking."""
        code = """
public void test() {
    List<String> list = new ArrayList<>();
    boolean b = list.size() < 10;
    String str = "?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?";
}
"""
        result = get_java_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("test", result[0].name)
