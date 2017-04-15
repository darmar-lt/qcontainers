#include "include_defines.fi"

!===========================================
!
!===========================================
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

        !void function qlibc_copy_data_new_c(void* val_from, void** val_to_p, val_size)
        subroutine qlibc_copy_data_new_c(val_data, val_to_p, val_size) bind(c, name="qlibc_copy_data_new_c")
            use iso_c_binding, only: c_ptr, c_size_t
#ifndef USE_COMPILER_DIRECTIVE
            type(*) :: val_data
#else
#include "nocheck_val_data.fi"
#endif
            type(c_ptr) :: val_to_p
            integer(c_size_t), value :: val_size
        end subroutine

        !void qlibc_copy_string_new_c(char str_from[], char* str_to_p[], size_t str_f_len)
        subroutine qlibc_copy_string_new_c(str_from, str_to_p, str_f_len) bind(c, name="qlibc_copy_string_new_c")
            use iso_c_binding, only: c_ptr, c_size_t
            type(c_ptr), value :: str_from
            type(c_ptr) :: str_to_p
            integer(c_size_t), value :: str_f_len
        end subroutine

        !size_t qlibc_cstr_len_c(char str[])
        function qlibc_cstr_len_c(str) bind(c, name="qlibc_cstr_len_c")
            use iso_c_binding, only: c_size_t, c_ptr
            integer(c_size_t) :: qlibc_cstr_len_c
            type(c_ptr), value :: str
        end function

        ! void qlibc_free_c(void *obj)
        subroutine qlibc_free_c(obj) bind(c, name="qlibc_free_c")
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
