"""Cognitive Complexity tests for Fortran"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_fortran_cogc(source_code):
    """Analyze Fortran code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.f90", source_code
    ).function_list


class TestFortranCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for Fortran"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        function simple(x)
            integer :: x, simple
            simple = x + 1
        end function simple
        '''
        functions = get_fortran_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement with else should be CogC=2"""
        code = '''
        function check(x)
            integer :: x
            character(len=20) :: check
            if (x > 0) then  ! +1
                check = "positive"
            else             ! +1
                check = "non-positive"
            end if
        end function check
        '''
        functions = get_fortran_cogc(code)
        self.assertEqual(2, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        subroutine nested()
            integer :: i, j
            do i = 1, 10           ! +1
                do j = 1, 10       ! +2 (nesting=1)
                    if (i == j) then  ! +3 (nesting=2)
                        print *, i
                    end if
                end do
            end do
        end subroutine nested
        ! Total CogC = 6
        '''
        functions = get_fortran_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        function check(a, b, c, d, e)
            logical :: a, b, c, d, e
            integer :: check
            if (a .and. b .and. c) then  ! +1 for if, +1 for .and. sequence
                check = 1
            else if (d .or. e) then      ! +1 for elseif, +1 for .or. sequence
                check = 2
            else                         ! +1
                check = 0
            end if
        end function check
        ! Total CogC = 5
        '''
        functions = get_fortran_cogc(code)
        self.assertEqual(5, functions[0].cognitive_complexity)
