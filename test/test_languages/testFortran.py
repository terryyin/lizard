import unittest
from lizard import analyze_file, get_reader_for
from lizard_languages import FortranReader


def get_fortran_fileinfo(source_code):
    return analyze_file.analyze_source_code('a.f90', source_code)


def get_fortran_function_list(source_code):
    return get_fortran_fileinfo(source_code).function_list


class TestFortran(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(FortranReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_ext(self):
        self.assertEqual(get_reader_for('a.f90'), FortranReader)

    def test_empty(self):
        self.check_tokens([], '')
        result = get_fortran_function_list('')
        self.assertEqual(0, len(result))

    def test_composed_tokens(self):
        self.check_tokens(
            ['.or.', '.and.', '.OR.', '.AND.', ' ', 'end', ' ', 'end if', ' ', 'end  type', ' ', 'end\tdo', ' ', 'else if'],
            '.or..and..OR..AND. end end if end  type end\tdo else if')

    def test_subroutine(self):
        result = get_fortran_function_list('''
        SUBROUTINE test(a, b)
            REAL :: a
            REAL :: b
        END SUBROUTINE test
        subroutine test2
        endsubroutine test2
        ''')
        self.assertEqual(2, len(result))
        self.assertEqual('test', result[0].name)
        self.assertEqual('test( a , b )', result[0].long_name)
        self.assertEqual('test2', result[1].name)
        self.assertEqual('test2( )', result[1].long_name)

    def test_function(self):
        result = get_fortran_function_list('''
        FUNCTION test(a, b)
            REAL :: a
            REAL :: b
        END FUNCTION test
        function test2
        endfunction test2
        ''')
        self.assertEqual(2, len(result))
        self.assertEqual('test', result[0].name)
        self.assertEqual('test( a , b )', result[0].long_name)
        self.assertEqual('test2', result[1].name)
        self.assertEqual('test2( )', result[1].long_name)

    def test_module(self):
        result = get_fortran_function_list('''
        module test
            interface operator (+)
                module procedure concat
            end interface
            
            subroutine test2
            endsubroutine test2
        end module test
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual('test::test2', result[0].name)
        self.assertEqual('test::test2( )', result[0].long_name)
        self.assertEqual(1, result[0].top_nesting_level)

    def test_module_procedure(self):
        result = get_fortran_function_list('''
        module test
            subroutine test2
            endsubroutine test2
        end module test
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual('test::test2', result[0].name)
        self.assertEqual('test::test2( )', result[0].long_name)
        self.assertEqual(1, result[0].top_nesting_level)

    def test_if(self):
        result = get_fortran_function_list('''
        subroutine test
            if (a) call sub(a)
            if (b) then
                call sub(b)
            end if
        endsubroutine test
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_if_else(self):
        result = get_fortran_function_list('''
        subroutine test
            if (a) then
                call sub(a)
            else
                call sub(-a)
            end if
            if (b) then
                call sub(b)
            else  if (c) then
                call sub(c)
            end if
        endsubroutine test
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_complexity(self):
        result = get_fortran_function_list('''
        subroutine test
            if (a .AND. b) then
                do b = 1, 10
                    select case (b)
                    case (1)
                        do xxx
                            do xxx
                                call sub()
                            end do
                        enddo
                    case (2)
                        call sub()
                    endselect
                end do
            else if (a .OR. b) then
                sub()
            endif
        endsubroutine test
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(10, result[0].cyclomatic_complexity)

    def test_labeled_do(self):
        result = get_fortran_function_list('''
        subroutine test
            DIMENSION U(100)
            S = 0.0 
            DO 1 J = 1, 100 
                    S = S + U(J) 
                    IF ( S .GE. 1000000 ) GO TO 2 
        1   CONTINUE 
            STOP 
        2   CONTINUE 
        endsubroutine test
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_macro_branched(self):
        result = get_fortran_function_list('''
        #ifdef TEST
            subroutine test
        #elif TEST2
            #ifndef TEST3
                subroutine test3
                end subroutine
            #elif TEST4
                subroutine test4
                end subroutine
            #endif
            subroutine test2
        #elif TEST5
            subroutine test5
        #else
            subroutine test6
        #endif
            end subroutine
        #if true
            subroutine test7
            end subroutine
        #endif
        ''')
        self.assertEqual(2, len(result))
        self.assertEqual('test', result[0].name)
        self.assertEqual('test7', result[1].name)

    def test_case_insensitive_tokens(self):
        '''Test that tokens are matched case-insensitively'''
        result = get_fortran_function_list('''
        subroutine test
            If (a) Then
                CALL sub(a)
            elseIF (b) THEN
                call SUB(b)
            END IF
        ENDsubroutine test
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual('test', result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def xtest_submodule_parsing(self):
        '''Test that submodules are correctly parsed'''
        result = get_fortran_function_list('''
        submodule (parent) child
            module procedure sub1
                integer :: x
            end procedure

            module procedure sub2
                real :: y
            end procedure
        end submodule
        ''')
        self.assertEqual(2, len(result))
        self.assertEqual('submodule::sub1', result[0].name)
        self.assertEqual('submodule::sub2', result[1].name)

    def test_module_procedure_parsing(self):
        '''Test that module procedures are correctly parsed'''
        result = get_fortran_function_list('''
        module mymod
            interface
                module subroutine sub1(x)
                    integer :: x
                end subroutine

                module function func1(y) result(z)
                    real :: y, z
                end function
            end interface
        end module
        ''')
        self.assertEqual(2, len(result))
        self.assertEqual('mymod::sub1', result[0].name)
        self.assertEqual('mymod::func1', result[1].name)

    def test_case_sensitivity_with_module_procedures(self):
        '''Test that module procedures are found regardless of case'''
        result = get_fortran_function_list('''
        MODULE mymod
            INTERFACE
                module SUBROUTINE sub1(x)
                    integer :: x
                end SUBROUTINE

                MODULE function func1(y)
                    real :: y
                end function
            end INTERFACE
        END MODULE

        SUBMODULE (mymod) mysubmod
            module procedure SUB1
                x = 1
            end procedure

            module procedure FUNC1
                func1 = y * 2
            end procedure
        end SUBMODULE
        ''')
        self.assertEqual(2, len(result))
        self.assertIn('mymod::sub1', [f.name for f in result])
        self.assertIn('mymod::func1', [f.name for f in result])

    def test_procedure_decorators(self):
        '''Test that procedures with decorators are correctly parsed'''
        result = get_fortran_function_list('''
        module mymod
            interface
                ! These should be found due to decorators
                module recursive subroutine sub1(x)
                    integer :: x
                end subroutine

                module elemental function func1(y)
                    real :: y
                end function

                ! This might be missed due to lack of decorator
                module function func2(z)
                    complex :: z
                end function
            end interface
        end module
        ''')
        self.assertEqual(3, len(result))
        self.assertIn('mymod::recursive::sub1', [f.name for f in result], "Recursive procedure not found")  # Recursive one
        self.assertIn('mymod::recursive::elemental::func1', [f.name for f in result], "Elemental procedure not found")  # Elemental one
        self.assertIn('mymod::recursive::elemental::func2', [f.name for f in result], "Non-decorated procedure not found")  # Non-decorated one


class TestFortranCoverageGaps(unittest.TestCase):
    """Tests targeting previously-uncovered branches in lizard_languages/fortran.py."""

    def _funcs(self, code):
        return get_fortran_function_list(code)

    def test_program_block(self):
        # _state_global PROGRAM → _namespace
        code = (
            "PROGRAM hello\n"
            "  PRINT *, 'hi'\n"
            "END PROGRAM hello\n"
        )
        # PROGRAM is a namespace, not a function — no entry in function_list
        self.assertEqual([], self._funcs(code))

    def test_typed_function_prefix(self):
        # _ignore_var FUNCTION_NAME_TOKENS branch — INTEGER FUNCTION foo()
        code = (
            "INTEGER FUNCTION square(x)\n"
            "  INTEGER :: x\n"
            "  square = x * x\n"
            "END FUNCTION square\n"
        )
        funcs = self._funcs(code)
        names = [f.name for f in funcs]
        self.assertIn('square', names)

    def test_typed_var_no_function(self):
        # _ignore_var else branch — REAL :: x (just a var declaration)
        code = (
            "SUBROUTINE foo()\n"
            "  REAL :: x\n"
            "  x = 1.0\n"
            "END SUBROUTINE foo\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual('foo', funcs[0].name)

    def test_save_statement(self):
        # IGNORE_NEXT_TOKENS 'SAVE' → _ignore_next
        code = (
            "SUBROUTINE foo()\n"
            "  SAVE x\n"
            "  INTEGER :: x\n"
            "END SUBROUTINE foo\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))

    def test_data_statement(self):
        # IGNORE_NEXT_TOKENS 'DATA' → _ignore_next
        code = (
            "SUBROUTINE foo()\n"
            "  INTEGER :: x\n"
            "  DATA x /0/\n"
            "END SUBROUTINE foo\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))

    def test_recursive_function(self):
        # RESET_STATE_TOKENS 'RECURSIVE' → reset_state
        code = (
            "RECURSIVE FUNCTION fact(n) RESULT(r)\n"
            "  INTEGER :: n, r\n"
            "  IF (n <= 1) THEN\n"
            "    r = 1\n"
            "  ELSE\n"
            "    r = n * fact(n - 1)\n"
            "  END IF\n"
            "END FUNCTION fact\n"
        )
        funcs = self._funcs(code)
        names = [f.name for f in funcs]
        self.assertIn('fact', names)

    def test_block_construct(self):
        # _state_global BLOCK → _ignore_if_paren else branch
        code = (
            "SUBROUTINE foo()\n"
            "  BLOCK\n"
            "    INTEGER :: x\n"
            "    x = 1\n"
            "  END BLOCK\n"
            "END SUBROUTINE foo\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))

    def test_type_declaration(self):
        # _state_global TYPE → _type → _namespace
        code = (
            "MODULE m\n"
            "  TYPE :: point\n"
            "    REAL :: x, y\n"
            "  END TYPE point\n"
            "END MODULE m\n"
        )
        # TYPE is a namespace, not a function — no function_list entries
        self.assertEqual([], self._funcs(code))

    def test_type_with_attribute(self):
        # _type ',' branch → _namespace
        code = (
            "MODULE m\n"
            "  TYPE, PUBLIC :: point\n"
            "    REAL :: x\n"
            "  END TYPE point\n"
            "END MODULE m\n"
        )
        self.assertEqual([], self._funcs(code))

    def test_if_single_line(self):
        # _if_then else branch — IF without THEN (single-line form)
        code = (
            "SUBROUTINE foo(x)\n"
            "  INTEGER :: x\n"
            "  IF (x > 0) x = x + 1\n"
            "END SUBROUTINE foo\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        # Single-line IF should still bump CCN
        self.assertGreaterEqual(funcs[0].cyclomatic_complexity, 2)

    def test_submodule(self):
        # _state_global SUBMODULE branch + _module else path
        code = (
            "SUBMODULE (parent) child\n"
            "  CONTAINS\n"
            "  SUBROUTINE foo()\n"
            "    PRINT *, 'hi'\n"
            "  END SUBROUTINE foo\n"
            "END SUBMODULE child\n"
        )
        funcs = self._funcs(code)
        names = [f.name for f in funcs]
        self.assertTrue(any('foo' in n for n in names))

    # ------------------------------------------------------------------
    # Iteration 2 — reachable coverage gaps in fortran.py
    # See docs/pr0-findings.md for methodology.
    # ------------------------------------------------------------------

    def test_fixed_form_c_comment_line(self):
        # Target: fortran.py:62 — preprocess() C/*-style comment rewrite.
        # Fixed-form Fortran (pre-F90) uses 'C' or '*' in column 1 as a
        # comment marker; preprocess() rewrites it to '!' so downstream
        # tokenization treats it as a comment.
        # Counter-input MISSING: a C-style comment line must NOT be
        # mistaken for code inside the function body (cyclomatic must
        # stay at 1, not absorb the comment as a keyword).
        code = (
            "C this is a fixed-form comment\n"
            "      SUBROUTINE foo()\n"
            "* another fixed-form comment\n"
            "      END SUBROUTINE foo\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual('foo', funcs[0].name)
        self.assertEqual(1, funcs[0].cyclomatic_complexity)

    def test_block_construct_with_parenthesis(self):
        # Target: fortran.py:172 — _ignore_if_paren '(' branch.
        # Existing test_block_construct uses bare BLOCK (no parens),
        # which exits via the else branch at line 174.  This exercises
        # the paren branch by placing a '(' immediately after BLOCK.
        # Counter-input BOUNDARY: paired with test_block_construct,
        # both BLOCK forms must yield exactly one function named 'foo'
        # with cyclomatic 1 (the BLOCK itself must not add complexity).
        code = (
            "SUBROUTINE foo()\n"
            "  BLOCK (1)\n"
            "    INTEGER :: x\n"
            "    x = 1\n"
            "  END BLOCK\n"
            "END SUBROUTINE foo\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual('foo', funcs[0].name)
        self.assertEqual(1, funcs[0].cyclomatic_complexity)

    def test_type_kind_spec_declaration(self):
        # Target: fortran.py:232 — _type else branch.
        # TYPE(name) is the derived-type kind-spec syntax for a variable
        # declaration (not a type definition).  The '(' token fails the
        # alpha/',' /'::' check at line 229, falling through to the else
        # branch which resets state with the '(' re-emitted.
        # Counter-input: paired with existing test_type_declaration
        # (which uses 'TYPE :: point') and test_type_with_attribute
        # (which uses 'TYPE, PUBLIC'), covering the three _type branches.
        code = (
            "SUBROUTINE foo()\n"
            "  TYPE(point) :: p\n"
            "  p%x = 1.0\n"
            "END SUBROUTINE foo\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual('foo', funcs[0].name)

    def test_if_identifier_not_keyword(self):
        # Target: fortran.py:242 — _if else branch.
        # When 'IF' appears but is not followed by '(' it cannot be an
        # IF statement; the state machine must reset and reprocess the
        # current token through _state_global.
        #
        # Observed quirk: lizard's generic cyclomatic counter still
        # increments on every 'if' token regardless of whether the
        # Fortran state machine treats it as a keyword, so the two
        # 'if' occurrences below produce cyclomatic = 3 (1 + 2).  This
        # is independent of fortran.py and not in PR0 scope; the test
        # locks in the observed value so a regression in the _if else
        # branch (which currently reaches line 242) would still be
        # caught via the function-count / name assertions.
        code = (
            "SUBROUTINE foo()\n"
            "  INTEGER :: if\n"
            "  if = 1\n"
            "END SUBROUTINE foo\n"
        )
        funcs = self._funcs(code)
        self.assertEqual(1, len(funcs))
        self.assertEqual('foo', funcs[0].name)
        # Locks in current behavior; see note above.
        self.assertEqual(3, funcs[0].cyclomatic_complexity)
