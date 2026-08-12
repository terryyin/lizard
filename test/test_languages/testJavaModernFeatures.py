import unittest
from .java_helpers import get_java_function_list


class TestJavaModernFeatures(unittest.TestCase):

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

    def test_local_class_inside_method(self):
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
        self.assertEqual("Outer::method", result[1].name)
        self.assertEqual("Local::innerMethod", result[0].name)

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

    def test_anonymous_class_complexity(self):
        result = get_java_function_list("""
            class Demo {
                public void test() {
                    new Thread(new Runnable() {
                        @Override
                        public void run() {
                            if(true) {
                                System.out.println("Hello");
                            }
                        }
                        
                        public void testA() {
                            if(true) {
                                System.out.println("World!");
                            }
                        }
                    }).start();
                }
            }
        """)
        self.assertEqual(3, len(result))  # Should find test, run and testA methods
        main_function = next(f for f in result if f.name == "Demo::test")
        self.assertEqual(1, main_function.cyclomatic_complexity)  # Main function should have complexity of 1
        run_method = next(f for f in result if f.name.endswith("::run"))
        self.assertEqual(2, run_method.cyclomatic_complexity)  # run method has one if, so complexity 2
        testA_method = next(f for f in result if f.name.endswith("::testA"))
        self.assertEqual(2, testA_method.cyclomatic_complexity)  # testA has one if, so complexity 2
