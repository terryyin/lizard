# Separating Mixed Condition Concepts in Lizard

## Problem Statement

Currently, all language readers mix three conceptually different types of complexity-contributing constructs into a single `_conditions` set:

1. **Control flow keywords** (e.g., `if`, `while`, `for`, `catch`)
2. **Logical operators** (e.g., `&&`, `||`, `and`, `or`)
3. **Case/switch labels** (e.g., `case`, `when`)

While this mixing is "accidentally correct" for basic CCN counting (all add +1 to complexity), it is conceptually wrong and causes maintenance issues:

- Extensions like `lizardnonstrict` need to remove logical operators separately
- Extensions like `lizardmccabe` and `lizardmodified` need special handling for `case`
- Language implementers are confused about what to put in `_conditions`
- It's difficult to add features that treat these concepts differently

## Research Findings

### Current Usage in Code

**Base class (code_reader.py):**
```python
_conditions = {'if', 'for', 'while', '&&', '||', '?', 'catch', 'case'}
```

**How conditions are used:**
- `lizard.py:condition_counter()` checks if `token in conditions` and calls `context.add_condition()`
- Extensions that modify behavior:
  - `lizardnonstrict.py`: Removes `['&&', '||', 'and', 'or']` from conditions
  - `lizardmccabe.py`: Adds special state machine to subtract complexity for consecutive `case` statements
  - `lizardmodified.py`: Adds `switch` and subtracts `case` complexity
  - `lizardcomplextags.py`: Records all tokens that match conditions

### Language Variations

#### Symbol-based logical operators
- **C/C++/Java/C#/JavaScript/TypeScript/Kotlin/Rust/Scala/Swift/Solidity/GDScript**: `&&`, `||`
- **PHP**: `&&`, `||` (also has `and`, `or` with different precedence)
- **Go**: `&&`, `||` (inherited from base, no explicit override)

#### Word-based logical operators
- **Python**: `and`, `or`
- **Ruby/Lua**: `and`, `or`
- **Perl**: `and`, `or`, `&&`, `||` (both available)
- **PL/SQL**: `and`, `or` (case-insensitive: `AND`, `OR`)
- **Fortran**: `.and.`, `.or.` (case-insensitive: `.AND.`, `.OR.`)
- **Zig**: `and`, `or`, `orelse`
- **R**: `&&`, `||`, `&`, `|` (short-circuit vs element-wise)

#### Control flow keywords
All languages have their own set:
- Common: `if`, `for`, `while`, `catch`
- Variants: `elsif`/`elseif`/`elif`, `unless`, `until`, `foreach`
- Special: `guard` (Swift), `when` (Kotlin/Erlang), `case` (many)

#### Case/Switch handling
- Default: Each `case` adds +1 (lizard default)
- McCabe mode: Only first `case` adds +1
- Modified mode: Only `switch` adds +1, `case` adds +0

### Proper Domain Names

After analyzing the code and McCabe's theory, the proper domain concepts are:

1. **Control Flow Structures** (`control_flow_keywords`)
   - Keywords that create decision points: `if`, `for`, `while`, `catch`
   - Each creates a new execution path

2. **Logical Operators** (`logical_operators`)
   - Operators that combine conditions: `&&`, `||`, `and`, `or`
   - Each adds an additional decision point within a condition
   - Note: Some languages have multiple forms (R has `&`/`|` and `&&`/`||`)

3. **Case Labels** (`case_keywords`)
   - Keywords for switch/case branches: `case`, `when`
   - Special handling needed for different counting modes

4. **Ternary Operators** (`ternary_operators`)
   - Conditional expressions: `?`, `??`, `?:`
   - Could be grouped with control flow or kept separate

## Potential Bugs Identified

### 1. R Language - Element-wise vs Short-circuit operators

**Issue**: R has both:
- `&`, `|` - element-wise logical operators (vectorized)
- `&&`, `||` - short-circuit logical operators (scalar)

Currently both are treated the same in `_conditions`, but element-wise operators in vectorized operations might not represent decision points in the traditional sense.

**Example**:
```r
# This should add CCN for && (control flow decision)
if (x > 0 && y > 0) { ... }

# This might not be a control flow decision (vectorized operation)
results <- (x > 0) & (y > 0)  # Element-wise operation on vectors
```

### 2. Languages without explicit logical operator separation

Most languages correctly include their logical operators, but the conceptual mixing makes it unclear whether:
- All logical operators are included
- Operators are correctly categorized
- New operators are added consistently

### 3. Ternary operator handling

The `?` operator is in `_conditions` but has special semantics:
- In C-like languages: `condition ? true_value : false_value`
- In Erlang: different meaning
- Could be confused with nullable types (`Type?` in Kotlin, TypeScript)

### 4. Go language

Go inherits default `_conditions` including `case`, but doesn't override it. Need to verify if this is correct behavior.

## Proposed Solution

### 1. Separate condition types in base class

**code_reader.py:**
```python
class CodeReader:
    # Separate the concepts
    _control_flow_keywords = {'if', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}
    
    # Backward compatibility - combine all by default
    @classmethod
    def _build_conditions(cls):
        return (cls._control_flow_keywords | 
                cls._logical_operators | 
                cls._case_keywords | 
                cls._ternary_operators)
    
    def __init__(self, context):
        self.parallel_states = []
        self.context = context
        # Build the combined set
        self.conditions = copy(self._build_conditions())
        # Also expose individual sets for extensions
        self.control_flow_keywords = copy(self._control_flow_keywords)
        self.logical_operators = copy(self._logical_operators)
        self.case_keywords = copy(self._case_keywords)
        self.ternary_operators = copy(self._ternary_operators)
```

### 2. Update all language readers

Each language reader should override the separate sets:

**Example - python.py:**
```python
class PythonReader(CodeReader, ScriptLanguageMixIn):
    _control_flow_keywords = {'if', 'elif', 'for', 'while', 'except', 'finally'}
    _logical_operators = {'and', 'or'}
    _case_keywords = set()  # Python uses if/elif, not case
    _ternary_operators = set()  # Python uses 'x if c else y', not ?
```

**Example - rust.py:**
```python
class RustReader(CLikeReader):
    _control_flow_keywords = {'if', 'for', 'while', 'catch', 'match', 'where'}
    _logical_operators = {'&&', '||'}
    _case_keywords = set()  # Rust uses match arms, not case
    _ternary_operators = {'?'}
```

### 3. Update extensions

**lizardnonstrict.py:**
```python
def __call__(self, tokens, reader):
    # More explicit - remove logical operators
    reader.conditions -= reader.logical_operators
    # Or even simpler:
    # reader.logical_operators.clear()
    return tokens
```

**lizardmccabe.py & lizardmodified.py:**
```python
# Can now specifically target case keywords
# Implementation becomes clearer with explicit case_keywords set
```

### 4. Maintain backward compatibility

- Keep `_conditions` as a class variable that will be deprecated
- If a language reader only defines `_conditions`, use it as before
- If it defines the new separate sets, use those
- Add deprecation warning for `_conditions` usage

### 5. Fix identified bugs

**R Language:**
- Separate `&`/`|` from `&&`/`||`
- Consider whether element-wise operators should count toward CCN
- Add tests to verify correct behavior

## Implementation Plan

### Phase 1: Infrastructure (No behavior change)
1. Add new fields to `CodeReader` base class
2. Update `__init__` to build combined `conditions` set from new fields
3. Add backward compatibility for readers still using `_conditions`
4. Add deprecation warning (optional, can be later)

### Phase 2: Update all language readers
For each language in `lizard_languages/`:
1. Replace `_conditions` with the four separate sets
2. Verify existing tests still pass
3. Add comments explaining choices if non-obvious

### Phase 3: Update extensions
1. Update `lizardnonstrict.py` to use `logical_operators`
2. Update `lizardmccabe.py` to use `case_keywords`
3. Update `lizardmodified.py` to use `case_keywords`
4. Update `lizardcomplextags.py` if needed

### Phase 4: Write tests to reproduce bugs (TDD approach)
1. Write test for R language element-wise vs short-circuit operators
2. Write test for Rust incorrect 'case' keyword
3. Write test for Erlang '?' operator meaning
4. Write test for Perl duplicate definitions
5. Write tests for ST, TTCN, GDScript edge cases
6. Run tests to verify bugs are reproduced

### Phase 5: Fix bugs (after tests are written)
1. Fix R language element-wise vs short-circuit operators
2. Fix Rust 'case' keyword bug
3. Fix Erlang '?' operator handling
4. Fix Perl duplicate definitions
5. Fix ST, TTCN, GDScript issues
6. Run tests until all pass

### Phase 6: Documentation
1. Update language implementation guide
2. Update contributor documentation
3. Add migration guide for custom readers

## Testing Strategy (TDD Approach)

### 1. Regression Testing
- All existing tests must pass after each phase
- Run full test suite after infrastructure changes
- Verify no behavior changes for existing functionality

### 2. Bug Reproduction Tests (Write First)
Before fixing any bug, write tests that:
- **R language**: Demonstrate element-wise (&, |) vs short-circuit (&&, ||) difference
- **Rust**: Show 'case' is incorrectly in _conditions
- **Erlang**: Show '?' operator meaning differs from ternary
- **Perl**: Expose duplicate _conditions definitions
- **ST/TTCN/GDScript**: Verify edge cases with logical operators and 'else'

### 3. Test-Driven Bug Fixes
For each bug:
1. Write test that reproduces the issue
2. Run test - it should fail or show incorrect behavior
3. Fix the bug
4. Run test - it should now pass
5. Run all tests - ensure no regression

### 4. Extension Testing
- Verify extensions work with new structure
- Test `lizardnonstrict` removes correct operators
- Test `lizardmccabe` handles case correctly
- Test `lizardmodified` handles switch/case correctly

### 5. Integration Testing
- Run on real codebases to verify no behavior change
- Test with various language combinations
- Verify backward compatibility with custom readers

## Benefits

1. **Clarity**: Clear separation of concepts makes code easier to understand
2. **Maintainability**: Extensions can target specific concept types
3. **Correctness**: Easier to verify each language has correct operators
4. **Extensibility**: Future extensions can leverage the separation
5. **Documentation**: Self-documenting code structure

## Risks and Mitigations

**Risk**: Breaking existing custom language readers
**Mitigation**: Maintain backward compatibility with `_conditions`

**Risk**: Introducing bugs during refactoring
**Mitigation**: Comprehensive test suite, phase-by-phase implementation

**Risk**: Disagreement on categorization
**Mitigation**: Document reasoning, allow language-specific overrides

## Open Questions

1. Should `?` (ternary) be in `_control_flow_keywords` or separate?
2. Should R's element-wise operators count toward CCN?
3. Should we deprecate `_conditions` immediately or in a future version?
4. What about language-specific operators like Zig's `orelse`?

## Next Steps

1. Review this document with maintainers
2. Get consensus on domain names and approach
3. Create TODO items for implementation phases
4. Begin Phase 1 implementation

