"""
Reproduction script for issue #443
This script attempts to reproduce the freezing behavior reported when
iterating through results after lizard.analyze with multiple void functions
containing multiple static_cast<...> calls.
"""

import tempfile
import os
from lizard import analyze

# Create test C++ code with multiple void functions containing multiple static_cast calls
# Including complex cases with nested templates, pointers, and other constructs
test_code = """
#include <vector>
#include <memory>

void func1() {
    int a = static_cast<int>(1.5);
    double b = static_cast<double>(2);
    long c = static_cast<long>(3.0);
    void* ptr = static_cast<void*>(&a);
}

void func2() {
    int x = static_cast<int>(4.5);
    double y = static_cast<double>(5);
    long z = static_cast<long>(6.0);
    void* ptr = static_cast<void*>(&x);
}

void func3() {
    int p = static_cast<int>(7.5);
    double q = static_cast<double>(8);
    long r = static_cast<long>(9.0);
    void* ptr = static_cast<void*>(&p);
}

void func4() {
    int* ptr1 = static_cast<int*>(malloc(sizeof(int)));
    char* ptr2 = static_cast<char*>(malloc(100));
    float val = static_cast<float>(3.14159);
}

void func5() {
    const int* cptr = static_cast<const int*>(&val);
    unsigned int uval = static_cast<unsigned int>(val);
    short sval = static_cast<short>(val);
}

// More complex cases with nested templates
void func6() {
    std::vector<int>* vec1 = static_cast<std::vector<int>*>(ptr);
    std::shared_ptr<std::vector<double>>* vec2 = static_cast<std::shared_ptr<std::vector<double>>*>(ptr);
    const std::vector<std::pair<int, int>>* vec3 = static_cast<const std::vector<std::pair<int, int>>*>(ptr);
}

void func7() {
    auto ptr1 = static_cast<std::vector<int>*>(malloc(sizeof(std::vector<int>)));
    auto ptr2 = static_cast<std::vector<double>*>(malloc(sizeof(std::vector<double>)));
    auto ptr3 = static_cast<std::vector<float>*>(malloc(sizeof(std::vector<float>)));
}

void func8() {
    Base* base = static_cast<Base*>(derived);
    Derived* derived = static_cast<Derived*>(base);
    const Base* cbase = static_cast<const Base*>(derived);
    volatile int* vint = static_cast<volatile int*>(&val);
}

void func9() {
    int val1 = static_cast<int>(static_cast<double>(x));
    long val2 = static_cast<long>(static_cast<float>(y));
    short val3 = static_cast<short>(static_cast<int>(z));
}

void func10() {
    auto lambda = [](int x) { return static_cast<double>(x); };
    int result = static_cast<int>(lambda(5));
    double val = static_cast<double>(result);
}
"""

# Create a temporary file with the test code
with tempfile.NamedTemporaryFile(mode='w', suffix='.cpp', delete=False) as f:
    f.write(test_code)
    temp_file = f.name

try:
    print("Analyzing file...")
    result = analyze([temp_file])
    
    print("Iterating through results...")
    count = 0
    for file_info in result:
        print(f"File: {file_info.filename}")
        print(f"Functions found: {len(file_info.function_list)}")
        for func in file_info.function_list:
            print(f"  - {func.name}: CCN={func.cyclomatic_complexity}")
            count += 1
    
    print(f"\nTotal functions analyzed: {count}")
    print("SUCCESS: No freezing occurred!")
    
finally:
    # Clean up
    if os.path.exists(temp_file):
        os.remove(temp_file)
