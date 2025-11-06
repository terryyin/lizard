import unittest
from lizard import analyze_file


def get_java_fileinfo(source_code):
    return analyze_file.analyze_source_code("a.java", source_code)


def get_java_function_list(source_code):
    return get_java_fileinfo(source_code).function_list


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

    def test_complex_java_class_method_count(self):
        code = """
public class GitRepository implements SCM {
    private static final int MAX_SIZE_OF_A_DIFF = 100000;
    private String path = null;
    
    public GitRepository(String path) {
        this.path = path;
    }
    
    public void setPath(String path) {
        this.path = path;
    }
    
    public void delete() {
        if (path != null) {
            System.out.println("Deleting: " + path);
        }
    }
    
    public void reset() {
        try {
            System.out.println("Resetting");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(4, len(result))  # Should find 4 methods: constructor, setPath, delete, reset

    def test_complex_java_class_with_annotations_and_generics(self):
        code = """
public class GitRepository implements SCM {
    private static final int MAX_SIZE_OF_A_DIFF = 100000;
    private String path = null;
    private CollectConfiguration collectConfig;
    
    @Override
    public List<ChangeSet> getChangeSets() {
        try (Git git = openRepository()) {
            List<ChangeSet> allCs;
            if (!firstParentOnly) allCs = getAllCommits(git);
            else allCs = firstParentsOnly(git);
            return allCs;
        } catch (Exception e) {
            throw new RuntimeException("error in getChangeSets for " + path, e);
        }
    }

    private List<ChangeSet> firstParentsOnly(Git git) {
        RevWalk revWalk = null;
        try {
            List<ChangeSet> allCs = new ArrayList<>();
            return allCs;
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            revWalk.close();
        }
    }

    @Override
    public List<Modification> getDiffBetweenCommits(String priorCommitHash, String laterCommitHash) {
        try (Git git = openRepository()) {
            RepositoryMining repo = git.getRepository();
            return new ArrayList<>();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private synchronized void deleteMMBranch(Git git) throws GitAPIException {
        List<Ref> refs = git.branchList().call();
        for (Ref r : refs) {
            if (r.getName().endsWith("mm")) {
                git.branchDelete().setBranchNames("mm").setForce(true).call();
                break;
            }
        }
    }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(4, len(result))  # Should find 4 methods: getChangeSets, firstParentsOnly, getDiffBetweenCommits, deleteMMBranch

    def test_very_complex_java_class_with_try_resources_and_nested_types(self):
        code = """
public class GitRepository implements SCM {
    private static final int MAX_SIZE_OF_A_DIFF = 100000;
    private String path = null;
    
    public SCMRepository info() {
        try (Git git = openRepository(); RevWalk rw = new RevWalk(git.getRepository())) {
            AnyObjectId headId = git.getRepository().resolve(Constants.HEAD);
            RevCommit root = rw.parseCommit(headId);
            rw.sort(RevSort.REVERSE);
            rw.markStart(root);
            RevCommit lastCommit = rw.next();
            String origin = git.getRepository().getConfig().getString("remote", "origin", "url");
            return new SCMRepository(this, origin, path, headId.getName(), lastCommit.getName());
        } catch (Exception e) {
            throw new RuntimeException("error", e);
        }
    }

    @Override
    public List<Modification> getDiffBetweenCommits(String priorCommitHash, String laterCommitHash) {
        try (Git git = openRepository()) {
            RepositoryMining repo = git.getRepository();
            AnyObjectId priorCommit = repo.resolve(priorCommitHash);
            AnyObjectId laterCommit = repo.resolve(laterCommitHash);
            List<DiffEntry> diffs = this.getDiffBetweenCommits(repo, priorCommit, laterCommit);
            return diffs.stream()
                .map(diff -> {
                    class LocalClass {
                        void localMethod() {}
                    }
                    try {
                        return this.diffToModification(repo, diff);
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                })
                .collect(Collectors.toList());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private List<DiffEntry> getDiffBetweenCommits(RepositoryMining repo, AnyObjectId parentCommit,
            AnyObjectId currentCommit) {
        try (DiffFormatter df = new DiffFormatter(DisabledOutputStream.INSTANCE)) {
            df.setBinaryFileThreshold(2 * 1024);
            df.setRepository(repo);
            df.setDiffComparator(RawTextComparator.DEFAULT);
            df.setDetectRenames(true);
            List<DiffEntry> diffs = df.scan(parentCommit, currentCommit);
            return diffs;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private Modification diffToModification(RepositoryMining repo, DiffEntry diff) throws IOException {
        ModificationType change = Enum.valueOf(ModificationType.class, diff.getChangeType().toString());
        String oldPath = diff.getOldPath();
        String newPath = diff.getNewPath();
        return new Modification(oldPath, newPath, change, "", "");
    }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(5, len(result))  # Should find 5 methods: info, getDiffBetweenCommits (2), diffToModification, and LocalClass::localMethod

    def test_try_with_resources(self):
        code = """
public class Test {
    public void methodWithTryResources() {
        try (Resource r2 = new Resource()) {
            r1.use();
            r2.use();
        }
    }
    
    public void anotherMethod() {
        System.out.println("test");
    }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(2, len(result))  # Should find both methods

    def test_wildcard_in_map_generics_ccn(self):
        """Test for issue #435: wildcard in Java Map generics should not increase CCN"""
        code = """
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.tuple.Triple;

public class TestWildcard {
  // correct: CCN=1
  void test1(Object obj) {
    boolean isList = obj instanceof List<?>;
    List<?> list1 = (List<?>) obj;
    List<String> list2 = (List<String>) obj;
    List<? extends CharSequence> list3 = (List<? extends CharSequence>) obj;
  }

  // correct: CCN=1
  void test2(Object obj) {
    boolean isMap = obj instanceof Map;
    Map map1 = (Map) obj;
    Map<String, String> map2 = (Map<String, String>) obj;
  }

  // incorrect: CCN=3, should be 1
  void test3(Object obj) {
    boolean isMap = obj instanceof Map<?, ?>;
  }

  // incorrect: CCN=5, should be 1
  void test4(Object obj) {
    Map<?, ?> map = (Map<?, ?>) obj;
  }

  // incorrect: CCN=5, should be 1
  void test5(Object obj) {
    Map<? extends CharSequence, ? extends CharSequence> map = (Map<? extends CharSequence, ? extends CharSequence>) obj;
  }

  // incorrect: CCN=5, should be 1
  void test6(Object obj) {
    Map<String, ?> map1 = (Map<String, ?>) obj;
    Map<?, String> map2 = (Map<?, String>) obj;
  }

  // incorrect: CCN=5, should be 1
  void test7(Object obj) {
    Map<String, ? extends CharSequence> map1 = (Map<String, ? extends CharSequence>) obj;
    Map<? extends CharSequence, String> map2 = (Map<? extends CharSequence, String>) obj;
  }

  // incorrect: CCN=7, should be 1
  void test8(Object obj) {
    Triple<?, ?, ?> triple = (Triple<?, ?, ?>) obj;
  }
}
"""
        result = get_java_function_list(code)
        self.assertEqual(8, len(result))

        # All functions should have CCN=1, as none have actual control flow
        for func in result:
            self.assertEqual(
                1, func.cyclomatic_complexity,
                f"Function {func.name} should have CCN=1, got "
                f"{func.cyclomatic_complexity}")


