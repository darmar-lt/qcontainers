
module qlibc_util_m
    use iso_c_binding
    implicit none

    interface
        ! void qtreetbl_copy_data_c(void *val_data_from, void *val_data_to, size_t size_data, bool freemem)
        subroutine qlibc_copy_data_c(val_data_from, val_data_to, size_data, freemem) bind(c)
            import :: c_ptr, c_size_t, c_bool
            type(c_ptr), value :: val_data_from
            type(c_ptr), value :: val_data_to
            integer(c_size_t), value :: size_data
            logical(c_bool), value   :: freemem
        end subroutine

        ! void qlibc_free_c(void *obj)
        subroutine qlibc_free_c(obj) bind(c)
            import :: c_ptr
            type(c_ptr), value :: obj
        end subroutine

    end interface

contains
    pure function f_c_string(f_string) result (c_string)
        use, intrinsic :: iso_c_binding, only: c_char, c_null_char
        implicit none
        character(len=*), intent(in) :: f_string
        character(len=1,kind=c_char) :: c_string(len_trim(f_string)+1)
        integer                      :: n, i

        n = len_trim(f_string)
        do i = 1, n
            c_string(i) = f_string(i:i)
        end do
        c_string(n + 1) = c_null_char

    end function

end module qlibc_util_m
