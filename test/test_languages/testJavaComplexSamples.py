import unittest
from .java_helpers import get_java_function_list


class TestJavaComplexSamples(unittest.TestCase):

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
        """Wildcard in Java Map generics should not increase CCN."""
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
