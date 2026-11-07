import unittest
from .java_helpers import get_java_function_list


class TestJavaClassBody(unittest.TestCase):

    def test_field_class_literal_does_not_break_catch(self):
        """Foo.class in a field initializer must not let 'catch' be parsed as a method."""
        code = """
public class A {
    private String x = A.class.getName();
    public void m() {
        try {} catch (Exception e) {}
    }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("A::m", result[0].name)

    def test_static_initializer_block_control_structures(self):
        """static { ... } inside a class must not treat if/while/for/switch as methods."""
        code = """
public class A {
    static {
        if (true) {}
        while (false) {}
        for (int i = 0; i < 1; i++) {}
        switch (0) {}
    }
    void m() {}
}
"""
        result = get_java_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("A::m", result[0].name)

    def test_double_brace_anonymous_initializer(self):
        """new Type() {{ ... }} instance initializer must not add spurious methods."""
        code = """
import java.util.*;
public class A {
    Map<String, String> abc(String key) {
        if (key != null) {
            return new HashMap() {
                {
                    put("res_code", "1");
                }
            };
        }
        return null;
    }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("A::abc", result[0].name)

    def test_static_block_after_field_with_brace_initializer(self):
        """static {...} after a field with `= {};` must not leak as a method."""
        code = """
public class LizardTest {
    private String[] unixCmd = {};
    static {
        if (true) {
        }
    }
    public void test1() {}
}
"""
        result = get_java_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("LizardTest::test1", result[0].name)

    def test_record_as_field_name_followed_by_annotated_method(self):
        """A field named `record` must not swallow the following method."""
        code = """
public class LizardTest {
    private String record;

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

    def test_method_named_record(self):
        """A method named `record` must be detected correctly."""
        code = """
public class LizardTest {
    private String record(String name) {
        if (name.equals("a")) {
        }
        for (int i = 0; i < 10; i++) {
        }
        return "";
    }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("LizardTest::record", result[0].name)
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_static_block_record_field_and_annotated_methods(self):
        """Static block + record field/method + annotation must report exactly two methods."""
        code = """
public class LizardTest {
    private String[] unixCmd = {};
    static {
        if (true) {
        }
    }

    private String record;

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

    private String record(String name) {
        if (name.equals("a")) {

        }
        for (int i = 0; i < 10; i++) {

        }
        return "";
    }
}
"""
        result = get_java_function_list(code)
        names = sorted(f.name for f in result)
        self.assertEqual(["LizardTest::record", "LizardTest::test1"], names)

    def test_record_as_variable_name(self):
        """'record' as a variable name should not be treated as the record keyword."""
        code = """
public class Example {
    public void process() {
        String record = "test";
        System.out.println(record);
    }
    
    public void anotherMethod() {
        System.out.println("hello");
    }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(2, len(result))
        self.assertEqual("Example::process", result[0].name)
        self.assertEqual("Example::anotherMethod", result[1].name)

    def test_statement_keywords_in_static_block_are_not_methods(self):
        """Control / statement keywords in a static block must not be reported as methods."""
        for code in (
            "static { if(){}; catch(){} }",
            "static { if (x) { } }",
            "static { while (x) { } }",
            "static { for (int i = 0; i < 1; i++) { } }",
            "static { switch (x) { } }",
            "static { synchronized (lock) { } }",
            "static { try { } catch (Exception e) { } }",
        ):
            self.assertEqual([], get_java_function_list(code), code)
