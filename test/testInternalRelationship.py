import unittest
from .testHelpers import get_cpp_function_list_with_extnesion
from lizard_ext.lizardinternalrelationship import LizardExtension as ir
from lizard_ext.lizardinternalrelationship import preprocess


def TestInternalRelationship():#(unittest.TestCase):

    def test_empty_source(self):
        result = get_cpp_function_list_with_extnesion("", ir())
        self.assertEqual(0, len(result))

    def test_a_c_function(self):
        result = get_cpp_function_list_with_extnesion("void f(){}", ir())
        self.assertEqual(1, len(result))
        self.assertEqual(set(), result[0].dependencies)

    def test_a_function_with_one_dependency(self):
        result = get_cpp_function_list_with_extnesion("int C::f(){a();}", ir())
        self.assertEqual({"a"}, result[0].dependencies)

    def test_a_function_with_keyword(self):
        result = get_cpp_function_list_with_extnesion("int C::f(){if(m)b();}", ir())
        self.assertEqual({"m", "b"}, result[0].dependencies)

    def test_with_local_variable(self):
        result = get_cpp_function_list_with_extnesion("int C::f(){int a;}", ir())
        self.assertEqual(set(), result[0].dependencies)

    def test_using_local_variable(self):
        result = get_cpp_function_list_with_extnesion("int C::f(){type a; a;}", ir())
        self.assertEqual(set(), result[0].dependencies)

    def test_scoped_name(self):
        result = get_cpp_function_list_with_extnesion("int C::f(){a::b();}", ir())
        self.assertEqual({"a::b"}, result[0].dependencies)

    def test_parameter(self):
        result = get_cpp_function_list_with_extnesion("void f(type a){a;}", ir())
        self.assertEqual(set(), result[0].dependencies)

    def xtest_xxx(self):
        result = get_cpp_function_list_with_extnesion("""
void CmDiagnosticsService::startService()
{
    TRACE_LOG_BEG(log_);
    auto func = limFacade_->fetchByClass(lim::MoFunc_l::className);

    if (!func)
    {
        INFO_LOG(log_) << "Failed to retrieve Func_L object.";
        limFacade_->subscribeForFunclCreation(
            core::types::bindWeak(shared_from_this(), &CmDiagnosticsService::funcCreateHandler),
            subscriptionFuncl_);
        return;
    }
    createAndInitializeCmDiagnostics(func);
    TRACE_LOG_END(log_);
}""", ir())
        self.assertEqual({"a::b"}, result[0].dependencies)

class TestPreprocessor(unittest.TestCase):

    def process(self, *tokens):
        return [x for x in preprocess(tokens)]

    def test_no_namespaces(self):
        self.assertEqual(["a", "b"], self.process("a", "b"))

    def test_global_namespaces(self):
        self.assertEqual(["::b"], self.process("::", "b"))

    def test_namespaces(self):
        self.assertEqual(["a::b"], self.process("a", "::", "b"))

    def test_namespaces_and_no_namepace(self):
        self.assertEqual(["a::b", "c"], self.process("a", "::", "b", "c"))

