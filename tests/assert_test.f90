module assert_test_m
    implicit none


    logical :: printok = .true.  ! print message on success
    logical :: stoperr = .false. ! terminate on error
    integer :: assert_nok = 0       ! number of passed tests
    integer :: assert_nerr = 0      ! number of tests with error
    integer :: assert_nok_all = 0   ! number of passed tests
    integer :: assert_nerr_all = 0  ! number of tests with error

    interface assert_equal
        module procedure assert_equal_int
        module procedure assert_equal_str
        module procedure assert_equal_real
        module procedure assert_equal_double
    end interface

    private
    public :: assert_new, assert_init, assert_equal, assert_true, assert_false, assert_print_summary, &
              assert_conclude
contains

    subroutine assert_new(print_ok, stop_err)
        logical, intent(in) :: print_ok, stop_err

        printok     = print_ok
        stoperr     = stop_err
        assert_nok  = 0
        assert_nerr = 0
    end subroutine

    subroutine assert_init()
        assert_nok_all = assert_nok_all + assert_nok
        assert_nerr_all = assert_nerr_all + assert_nerr
        assert_nok  = 0
        assert_nerr = 0
    end subroutine

    subroutine assert_equal_int(val1, val2)
        integer, intent(in) :: val1, val2

        if (val1 == val2) then
            assert_nok = assert_nok + 1
            if (printok) &
                print *, "OK val1=", val1, " val2=", val2
        else
            print *, "ERROR val1=", val1, " val2=", val2
            assert_nerr = assert_nerr + 1
            if (stoperr) stop "Execution terminated."
        end if
    end subroutine

    subroutine assert_equal_str(val1, val2)
        character(len=*), intent(in) :: val1, val2

        if (val1 == val2) then
            assert_nok = assert_nok + 1
            if (printok) &
                print *, "OK val1=", trim(val1), " val2=", trim(val2)
        else
            print *, "ERROR val1=", trim(val1), " val2=", trim(val2)
            assert_nerr = assert_nerr + 1
            if (stoperr) stop "Execution terminated."
        end if
    end subroutine

    subroutine assert_equal_real(val1, val2)
        real, intent(in) :: val1, val2

        if (val1 == val2) then
            assert_nok = assert_nok + 1
            if (printok) &
                print *, "OK val1=", val1, " val2=", val2
        else
            print *, "ERROR val1=", val1, " val2=", val2
            assert_nerr = assert_nerr + 1
            if (stoperr) stop "Execution terminated."
        end if
    end subroutine

    subroutine assert_equal_double(val1, val2)
        real(8), intent(in) :: val1, val2

        if (val1 == val2) then
            assert_nok = assert_nok + 1
            if (printok) &
                print *, "OK val1=", val1, " val2=", val2
        else
            print *, "ERROR val1=", val1, " val2=", val2
            assert_nerr = assert_nerr + 1
            if (stoperr) stop "Execution terminated."
        end if
    end subroutine

    subroutine assert_true(log_val)
        logical, intent(in) :: log_val

        if (log_val) then
            assert_nok = assert_nok + 1
            if (printok) &
                print *, "OK log_val=", log_val
        else
            print *, "ERROR log_val=", log_val
            assert_nerr = assert_nerr + 1
            if (stoperr) stop "Execution terminated."
        end if
    end subroutine

    subroutine assert_false(log_val)
        logical, intent(in) :: log_val

        if (.not. log_val) then
            assert_nok = assert_nok + 1
            if (printok) &
                print *, "OK log_val=", log_val
        else
            print *, "ERROR log_val=", log_val
            assert_nerr = assert_nerr + 1
            if (stoperr) stop "Execution terminated."
        end if
    end subroutine

    subroutine assert_print_summary()
        print *, ""
        print *, "************************************************"
        print *, "*    Passed tests: ", assert_nok
        print *, "*    Failed tests: ", assert_nerr
        print *, "************************************************"
        if (assert_nerr > 0) then
            print *, " WARNING: Some tests failed!"
        end if
    end subroutine

    subroutine assert_conclude()
        call assert_init()

        print *, ""
        print *, "************************************************"
        print *, "*----------------------------------------------*"
        print *, "*    CONCLUSION:"
        if (assert_nerr_all > 0) then
            print *, "*        Some tests failed! "
        else
            print *, "*        All tests passed! "
        end if
        print *, "*----------------------------------------------*"
        print *, "************************************************"
    end subroutine

end module
