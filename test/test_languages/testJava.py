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

    def test_record(self):
        result = get_java_function_list("""
            record Point(int x, int y) {
                public int sum() { return x + y; }
            }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("Point::sum", result[0].name)

    def test_sealed_class_with_permits_clause(self):
        result = get_java_function_list("""
            sealed class Shape permits Circle, Rectangle {
                void draw() {}
            }
            final class Circle extends Shape {
                void draw() {}
            }
        """)
        self.assertEqual(2, len(result))
        self.assertEqual("Shape::draw", result[0].name)
        self.assertEqual("Circle::draw", result[1].name)

    def test_sealed_interface_with_permits_clause(self):
        result = get_java_function_list("""
            sealed interface Vehicle permits Car, Truck {
                void move();
            }
            final class Car implements Vehicle {
                void move() {}
            }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("Car::move", result[0].name)

    def test_lambda_expression_simple(self):
        result = get_java_function_list("""
            class Test {
                void process() {
                    Runnable r = () -> System.out.println("Hello");
                }
            }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("Test::process", result[0].name)

    def test_lambda_expression_with_multiple_parameters(self):
        result = get_java_function_list("""
            class Calculator {
                void compute() {
                    BinaryOperator<Integer> add = (a, b) -> a + b;
                }
            }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("Calculator::compute", result[0].name)

    def test_method_reference_expression(self):
        result = get_java_function_list("""
            class Processor {
                void process() {
                    List<String> names = Arrays.asList("Alice", "Bob");
                    names.forEach(System.out::println);
                }
            }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("Processor::process", result[0].name)

    def test_record_compact_constructor(self):
        result = get_java_function_list("""
            record Person(String name, int age) {
                Person {
                    if (age < 0) throw new IllegalArgumentException();
                }
                void printInfo() {}
            }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("Person::printInfo", result[0].name)

    def test_enum_declaration_with_methods(self):
        result = get_java_function_list("""
            enum Day {
                MONDAY, TUESDAY;
                void printSchedule() {}
                static void printAll() {}
            }
        """)
        self.assertEqual(2, len(result))
        self.assertEqual("Day::printSchedule", result[0].name)
        self.assertEqual("Day::printAll", result[1].name)

    def xtest_local_class_inside_method(self):
        result = get_java_function_list("""
            class Outer {
                void method() {
                    class Local {
                        void innerMethod() {}
                    }
                    new Local().innerMethod();
                }
            }
        """)
        self.assertEqual(2, len(result))
        self.assertEqual("Outer::method", result[0].name)
        self.assertEqual("Local::innerMethod", result[1].name)

    def test_switch_expression_with_yield(self):
        result = get_java_function_list("""
            class SwitchDemo {
                int getNumber(String day) {
                    int numLetters = switch (day) {
                        case "MONDAY" -> 6;
                        case "TUESDAY" -> { yield 7; }
                        default -> 0;
                    };
                    return numLetters;
                }
            }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("SwitchDemo::getNumber", result[0].name)

    def test_pattern_matching_instanceof(self):
        result = get_java_function_list("""
            class PatternMatch {
                void process(Object obj) {
                    if (obj instanceof String s && s.length() > 0) {
                        System.out.println(s.toLowerCase());
                    }
                }
            }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("PatternMatch::process", result[0].name)
