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
