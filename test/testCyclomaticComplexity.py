from .testHelpers import get_cpp_function_list

import pytest

@pytest.mark.parametrize(
    "description,code,ccn",
    [("no condition", "int fun() {}", [1]),
     ("one condition", "int fun(){if(a){xx;}}", [2]),
     ("ternary operator", "int fun(){return (a)?b:c;}", [2]),
     ("forever_loop", "int fun(){for(;;){dosomething();}}", [2]),
     ("and operator", "int fun(){if(a&&b){xx;}}", [3]),
     ("if-else-if", "int fun(){if(a)b;else if (c) d;}", [3]),
     ("macro if-elif",
      """int main(){
            #ifdef A
            #elif (defined E)
            #endif
         }""",
      [3]),
     ("r-value reference in parameter", "int make(Args&&... args){}", [1]),
     ("r-value reference in body", "int f() {Args&& a=b;}", [1]),
     ("non r-value reference in body", "int f() {a && b==c;}", [2]),
     ("typedef with r-value reference", "int f() {typedef int&& rref;}", [1]),
     ("brace-less control structures",
      """void c() {
            if (a > -1 && b>= 0 )
                if(a != 0)
                    a = b;
         }""",
      [4]),
     ("function ref qualifier",
      "struct A { void foo() && { return bar() && baz(); } };",
      [2]),
     ("function const ref qualifier",
      "struct A { void foo() const && { return bar() && baz(); } };",
      [2]),
     ("two functions",
      """x c() {
            if (a && b) {}
        }
        x a() {
          inputs = c;
        }""",
      [3, 1])
     ])
def test_cpp_ccn(description, code, ccn):
    result = get_cpp_function_list(code)
    assert result, "Interpretation failure"
    assert len(result) == len(ccn), "Incorrect number of functions"
    for expected, value in zip(ccn, (x.cyclomatic_complexity for x in result)):
        assert value == expected, description
