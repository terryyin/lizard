# Separating Mixed Conditions - Research Summary

## Executive Summary

I've completed a comprehensive research of the condition mixing issue in Lizard's language readers. The research confirms that **all language readers mix conceptually different constructs** (control flow keywords, logical operators, and case labels) into a single `_conditions` set. While this is "accidentally correct" for basic CCN counting, it causes maintenance issues and has led to several bugs.

## Key Findings

### 1. Three Concepts Are Mixed
All 23+ language readers mix:
- **Control Flow Keywords**: `if`, `while`, `for`, `catch` - create decision points
- **Logical Operators**: `&&`, `||`, `and`, `or` - add branches within conditions  
- **Case/Switch Labels**: `case`, `when` - create switch branches

### 2. Why It's Accidentally Correct
The `condition_counter` in `lizard.py` simply checks `if token in conditions` and adds +1 to CCN. Since all three types should add +1 to complexity, mixing them works for basic counting.

### 3. Why It's Conceptually Wrong
- **Extensions need to treat them differently**: 
  - `lizardnonstrict.py` removes only logical operators
  - `lizardmccabe.py` adjusts only case counting
  - Hard to target specific types
- **Confusing for language implementers**: Unclear what should go in `_conditions`
- **Prevents advanced features**: Can't easily add features that distinguish between types

### 4. Bugs Identified

#### High Priority
1. **R Language** - Treats element-wise (`&`, `|`) same as short-circuit (`&&`, `||`) operators
   - Element-wise operators might not be control flow decisions
2. **Rust** - Includes `case` keyword but Rust doesn't have `case` (uses `match` arms)
3. **Erlang** - Includes `?` but in Erlang it's a macro operator, not ternary

#### Medium Priority  
4. **Perl** - Has TWO different `_conditions` definitions in the same file
5. **ST** - Missing AND/OR logical operators from `_conditions`
6. **TTCN & GDScript** - Include `else` keyword unusually

## Proposed Domain Names

Based on analysis of McCabe's theory and actual usage:

1. **`_control_flow_keywords`** - Keywords creating branches: `if`, `for`, `while`, `catch`
2. **`_logical_operators`** - Operators combining conditions: `&&`, `||`, `and`, `or`
3. **`_case_keywords`** - Switch/case labels: `case`, `when`
4. **`_ternary_operators`** - Conditional expressions: `?`, `??`, `?:`

## Recommended Approach

### Phase-by-Phase Refactoring (TDD Approach)
1. **Phase 1**: Add new fields to base class, maintain backward compatibility
2. **Phase 2-3**: Update all 23+ language readers (grouped by similarity)
3. **Phase 4**: Update extensions to use new structure
4. **Phase 5**: Write tests to reproduce identified bugs (TDD: test first!)
5. **Phase 6**: Fix bugs after tests are written
6. **Phase 7**: Documentation updates

### Key Design Decisions
- **Maintain backward compatibility**: Old `_conditions` still works
- **No behavior change**: All tests must pass during refactoring
- **Clear separation**: Each construct type in its own set
- **Gradual migration**: One language at a time

## Impact Analysis

### Languages Affected
- **23+ language readers** need updates
- **4 extensions** need updates
- **0 behavior changes** for users (backward compatible)

### Testing Requirements (TDD Approach)
- All existing tests must pass after each phase
- **Write tests first** to reproduce each identified bug
- Tests should fail before fixes, pass after fixes
- Integration tests with real codebases
- Verify backward compatibility with custom readers

## Benefits

1. **Maintainability**: Clear separation makes code easier to understand
2. **Correctness**: Easier to verify each language has correct operators
3. **Extensibility**: Extensions can target specific types
4. **Documentation**: Self-documenting structure
5. **Bug Prevention**: Clearer categorization prevents future bugs

## Documents Created

1. **`ongoing/separate-conditions-refactoring.md`**
   - Detailed problem statement and solution design
   - Implementation phases
   - Testing strategy
   - Open questions

2. **`ongoing/language-conditions-inventory.md`**
   - Complete inventory of all 23+ languages
   - Categorization of each token in `_conditions`
   - Bug identification with priorities
   - Summary statistics

3. **`ongoing/todo_list.md`**
   - Updated with 50+ actionable tasks
   - Organized by implementation phase
   - Includes bug fixes and testing tasks

## Next Steps

### Immediate Actions
1. Review research findings and approach
2. Get consensus on domain names
3. Decide on backward compatibility strategy
4. Begin Phase 1 implementation (infrastructure)

### Questions for Decision
1. Should we deprecate `_conditions` immediately or later?
2. How to handle R's element-wise operators? (count or not?)
3. Should ternary operators be separate or merged with control flow?
4. Timeline for completion?

## Conclusion

This is a **high-value, low-risk refactoring**:
- ✅ Improves code quality significantly
- ✅ Fixes real bugs
- ✅ Maintains backward compatibility  
- ✅ No user-visible behavior changes
- ✅ Clear implementation path

The research is complete and implementation can proceed systematically through the defined phases.

