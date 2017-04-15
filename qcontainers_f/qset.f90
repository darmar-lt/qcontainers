
!===========================================
!
!===========================================
module qset_interfaces_m
    implicit none

    interface


        !Set *set_new(SetHashFunc hash_func, SetEqualFunc equal_func)
        function set_new_c(hash_func, equal_func) bind(c, name="set_new")
            use iso_c_binding, only: c_ptr, c_funptr
            type(c_ptr) :: set_new_c
            type(c_funptr), value :: hash_func
            type(c_funptr), value :: equal_func
        end function

        !void set_register_free_function(Set *set, SetFreeFunc free_func)
        subroutine set_register_free_function_c(set, free_func) bind(c, name="set_register_free_function")
            use iso_c_binding, only: c_ptr, c_funptr
            type(c_ptr), value :: set
            type(c_funptr), value :: free_func
        end subroutine

        !void set_free(Set *set)
        subroutine set_free_c(set) bind(c, name="set_free")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: set
        end subroutine

        !int set_insert(Set *set, SetValue data)
        function set_insert_c(set, val_data_p) bind(c, name="set_insert")
            use iso_c_binding, only: c_int, c_ptr
            integer(c_int) :: set_insert_c
            type(c_ptr), value :: set
            type(c_ptr), value :: val_data_p
        end function

        !int set_remove(Set *set, SetValue data)
        function set_remove_c(set, val_data_p) bind(c, name="set_remove")
            use iso_c_binding, only: c_int, c_ptr
            integer(c_int) :: set_remove_c
            type(c_ptr), value :: set
            type(c_ptr), value :: val_data_p
        end function

        !int set_query(Set *set, SetValue data)
        function set_query_c(set, val_data_p) bind(c, name="set_query")
            use iso_c_binding, only: c_int, c_ptr
            integer(c_int) :: set_query_c
            type(c_ptr), value :: set
            type(c_ptr), value :: val_data_p
        end function

        ! void set_iterate(Set *set, SetIterator *iter)
        subroutine set_iterate_c(set, qobj) bind(c, name="set_iterate")
            use iso_c_binding
            type(c_ptr), value :: set
            type(c_ptr), value :: qobj
        end subroutine

        !int set_iter_has_more(SetIterator *iterator);
        function set_iter_has_more_c(qobj) bind(c, name="set_iter_has_more")
            use iso_c_binding, only: c_ptr, c_int
            integer(c_int) :: set_iter_has_more_c
            type(c_ptr), value :: qobj
        end function

        !SetValue set_iter_next(SetIterator *iterator);
        function set_iter_next_c(qobj) bind(c, name="set_iter_next")
            use iso_c_binding, only: c_ptr
            type(c_ptr) :: set_iter_next_c
            type(c_ptr), value :: qobj
        end function

    end interface

    interface
        !int int8_equal(void *vlocation1, void *vlocation2)
        function int8_equal_c(vlocation1, vlocation2) bind(c, name="int8_equal")
            use iso_c_binding, only: c_int, c_ptr
            integer(c_int) :: int8_equal_c
            type(c_ptr), value :: vlocation1
            type(c_ptr), value :: vlocation2
        end function

        !int int16_equal(void *vlocation1, void *vlocation2)
        function int16_equal_c(vlocation1, vlocation2) bind(c, name="int16_equal")
            use iso_c_binding, only: c_int, c_ptr
            integer(c_int) :: int16_equal_c
            type(c_ptr), value :: vlocation1
            type(c_ptr), value :: vlocation2
        end function

        !int int32_equal(void *vlocation1, void *vlocation2)
        function int32_equal_c(vlocation1, vlocation2) bind(c, name="int32_equal")
            use iso_c_binding, only: c_int, c_ptr
            integer(c_int) :: int32_equal_c
            type(c_ptr), value :: vlocation1
            type(c_ptr), value :: vlocation2
        end function

        !int int64_equal(void *vlocation1, void *vlocation2)
        function int64_equal_c(vlocation1, vlocation2) bind(c, name="int64_equal")
            use iso_c_binding, only: c_int, c_ptr
            integer(c_int) :: int64_equal_c
            type(c_ptr), value :: vlocation1
            type(c_ptr), value :: vlocation2
        end function

        !int string_equal(void *string1, void *string2)
        function string_equal_c(string1, string2) bind(c, name="string_equal")
            use iso_c_binding, only: c_int, c_ptr
            integer(c_int) :: string_equal_c
            type(c_ptr), value :: string1
            type(c_ptr), value :: string2
        end function

        !unsigned int int8_hash(void *vlocation)
        function int8_hash_c(vlocation) bind(c, name="int8_hash")
            use iso_c_binding, only: c_size_t, c_ptr
            integer(c_size_t) :: int8_hash_c
            type(c_ptr), value :: vlocation
        end function

        !unsigned int int16_hash(void *vlocation)
        function int16_hash_c(vlocation) bind(c, name="int16_hash")
            use iso_c_binding, only: c_size_t, c_ptr
            integer(c_size_t) :: int16_hash_c
            type(c_ptr), value :: vlocation
        end function

        !unsigned int int32_hash(void *vlocation)
        function int32_hash_c(vlocation) bind(c, name="int32_hash")
            use iso_c_binding, only: c_size_t, c_ptr
            integer(c_size_t) :: int32_hash_c
            type(c_ptr), value :: vlocation
        end function

        !unsigned int int64_hash(void *vlocation)
        function int64_hash_c(vlocation) bind(c, name="int64_hash")
            use iso_c_binding, only: c_size_t, c_ptr
            integer(c_size_t) :: int64_hash_c
            type(c_ptr), value :: vlocation
        end function

        !unsigned int string_hash(void *string)
        function string_hash_c(string) bind(c, name="string_hash")
            use iso_c_binding, only: c_size_t, c_ptr
            integer(c_size_t) :: string_hash_c
            type(c_ptr), value :: string
        end function

    end interface

end module

!===========================================
!
!===========================================
module qset_util_m
    use iso_c_binding
    implicit none

    interface

        !SetIterator* qu_set_new_iterator_c()
        type(c_ptr) function qu_set_new_iterator_c() bind(c, name="qu_set_new_iterator_c")
            use iso_c_binding, only: c_ptr
        end function

        !void qu_set_toarray_c(Set *set, void* array, size_t size_data)
        subroutine qu_set_toarray_c(set, array, size_data) bind(c, name="qu_set_toarray_c")
            use iso_c_binding, only: c_ptr, c_size_t
            type(c_ptr), value :: set
            type(*), dimension(*) :: array
            integer(c_size_t), value :: size_data
        end subroutine

        !void qu_set_toarray_str_c(Set *set, void* str_array, size_t size_data) {
        subroutine qu_set_toarray_str_c(set, str_array, size_data) bind(c, name="qu_set_toarray_str_c")
            use iso_c_binding, only: c_ptr, c_size_t
            type(c_ptr), value :: set
            type(c_ptr), value :: str_array
            integer(c_size_t), value :: size_data
        end subroutine

        !int qu_set_size_c(Set *set)
        function qu_set_size_c(set) bind(c, name="qu_set_size_c")
            use iso_c_binding, only: c_int, c_ptr
            integer(c_int) :: qu_set_size_c
            type(c_ptr), value :: set
        end function

        !void qu_set_copy_c(Set *to, Set *from, size_t val_size, bool is_str)
        subroutine qu_set_copy_c(to, from, val_size, is_str)  bind(c)
            use iso_c_binding, only: c_ptr, c_size_t, c_bool
            type(c_ptr), value :: to
            type(c_ptr), value :: from
            integer(c_size_t)  :: val_size
            logical(c_bool)    :: is_str
        end subroutine

    end interface

end module

!===========================================
!
!===========================================
module qset_m
    use iso_c_binding
    use qset_interfaces_m
    use qset_util_m
    use qlibc_util_m
    implicit none


    type, public :: qset_t
        private
        type(c_ptr)       :: q_cp = c_null_ptr
        integer(c_size_t) :: size_data
        logical(c_bool)   :: is_string_set
        type(c_funptr)    :: fun_hash
        type(c_funptr)    :: fun_equal

        logical :: was_init = .false.
    contains
        final :: qset_free
        procedure, private :: qset_new_int8, qset_new_int16, qset_new_int32
        procedure, private :: qset_new_int64, qset_new_char
        generic :: new => qset_new_int8, qset_new_int16, qset_new_int32, &
                            qset_new_int64, qset_new_char                    !< Constructor of the container.
        procedure :: put => qset_put                                         !< Put a value to the set.
        procedure :: putstr => qset_putstr                                   !< Put a character string value to the set.
        procedure :: remove => qset_remove                                   !< Remove a value from the set.
        procedure :: removestr => qset_removestr                             !< Remove a string from the set.
        procedure :: size => qset_size                                       !< Retrieve the number of entries in the set.
        procedure :: has => qset_has                                         !< Does the set has a particular value?
        procedure :: hasstr => qset_hasstr                                   !< Does the set has a string value?
        procedure :: getnext => qset_getnext                                 !< Get next element.
        procedure :: toarray => qset_toarray                                 !< Returns the array containing all the elements in this set.
        procedure :: toarraystr => qset_toarraystr                           !< Returns the character array containing all the character strings in this set.
        procedure :: clear => qset_clear                                     !< Removes all values from this list.
    end type

    type, public :: qset_obj_t
        private
        type(c_ptr) :: qobj = c_null_ptr
        logical :: wasInit = .false.
        type(c_ptr) :: data_p
        integer(c_size_t) :: size_data
    contains
        final :: qset_objfree
        procedure :: init => qset_objinit          !< Initialize the object. Should be called before the use of the object.
        procedure :: getdata => qset_getdata       !< Get data from the object.
        procedure :: getdatastr => qset_getdatastr !< Get string data from the object.
    end type

    interface assignment(=)
        module procedure qset_copy
        module procedure qset_obj_copy
    end interface

    private
    public :: assignment(=)

contains


    !> @brief qset constructor for storing of integer(c_int8_t)
    !!
    subroutine qset_new_int8(self, mold)
        class(qset_t) :: self
        integer(c_int8_t), intent(in) :: mold

        self%is_string_set = .false.
        self%fun_equal = c_funloc(int8_equal_c)
        self%fun_hash  = c_funloc(int8_hash_c)
        self%q_cp = set_new_c(self%fun_hash, self%fun_equal)
        self%size_data = c_sizeof(mold)
        call set_register_free_function_c(self%q_cp, c_funloc(qlibc_free_c))

        self%was_init = .true.
    end subroutine

    !> @brief qset constructor for storing of integer(c_int16_t)
    !!
    subroutine qset_new_int16(self, mold)
        class(qset_t) :: self
        integer(c_int16_t), intent(in) :: mold

        self%is_string_set = .false.
        self%fun_equal = c_funloc(int16_equal_c)
        self%fun_hash  = c_funloc(int16_hash_c)
        self%q_cp = set_new_c(self%fun_hash, self%fun_equal)
        self%size_data = c_sizeof(mold)
        call set_register_free_function_c(self%q_cp, c_funloc(qlibc_free_c))

        self%was_init = .true.
    end subroutine

    !> @brief qset constructor for storing of integer(c_int32_t)
    !!
    subroutine qset_new_int32(self, mold)
        class(qset_t) :: self
        integer(c_int32_t), intent(in) :: mold

        self%is_string_set = .false.
        self%fun_equal = c_funloc(int32_equal_c)
        self%fun_hash  = c_funloc(int32_hash_c)
        self%q_cp = set_new_c(self%fun_hash, self%fun_equal)
        self%size_data = c_sizeof(mold)
        call set_register_free_function_c(self%q_cp, c_funloc(qlibc_free_c))

        self%was_init = .true.
    end subroutine

    !> @brief qset constructor for storing of integer(c_int64_t)
    !!
    subroutine qset_new_int64(self, mold)
        class(qset_t) :: self
        integer(c_int64_t), intent(in) :: mold

        self%is_string_set = .false.
        self%fun_equal = c_funloc(int64_equal_c)
        self%fun_hash  = c_funloc(int64_hash_c)
        self%q_cp = set_new_c(self%fun_hash, self%fun_equal)
        self%size_data = c_sizeof(mold)
        call set_register_free_function_c(self%q_cp, c_funloc(qlibc_free_c))

        self%was_init = .true.
    end subroutine

    !> @brief qset constructor for storing of character strings
    !!
    subroutine qset_new_char(self, mold)
        class(qset_t) :: self
        character(len=*), intent(in) :: mold

        self%is_string_set = .true.
        self%fun_equal = c_funloc(string_equal_c)
        self%fun_hash  = c_funloc(string_hash_c)
        self%q_cp = set_new_c(self%fun_hash, self%fun_equal)
        self%size_data = len(mold)
        call set_register_free_function_c(self%q_cp, c_funloc(qlibc_free_c))

        self%was_init = .true.
    end subroutine

    !> @brief  Destructor of the qset.
    !!
    subroutine qset_free(self)
        type(qset_t), intent(inout) :: self

        if (.not. self%was_init) then
            self%q_cp = c_null_ptr
            self%was_init = .true.
        end if

        if (c_associated(self%q_cp)) then
            call set_free_c(self%q_cp)
            self%q_cp = c_null_ptr
        end if
    end subroutine

    !> @brief  Put a value to the set.
    !!
    subroutine qset_put(self, val_data, success)
        class(qset_t), intent(inout) :: self
        type(*), intent(in) :: val_data
        logical, optional, intent(out) :: success

        type(c_ptr) ::  vd_new_p
        integer :: res

        call qlibc_copy_data_new_c(val_data, vd_new_p, self%size_data)

        res = set_insert_c(self%q_cp, vd_new_p)
        if (res == 0) then
            ! free allocated memory
            call qlibc_free_c(vd_new_p)
        end if

        if (present(success)) then
            if (res == 0) then
                success = .false.
            else
                success = .true.
            end if
        end if
    end subroutine

    !> @brief  Put a character string value to the set.
    !!
    subroutine qset_putstr(self, str_data, success)
        class(qset_t), intent(inout) :: self
        character(len=*), target, intent(in) :: str_data
        logical, optional, intent(out) :: success

        type(c_ptr) :: cstr_new_p
        integer :: res

        call qlibc_copy_string_new_c(c_loc(str_data), cstr_new_p, len_trim(str_data,kind=c_size_t))

        res = set_insert_c(self%q_cp, cstr_new_p)
        if (res == 0) then
            ! free allocated memory
            call qlibc_free_c(cstr_new_p)
        end if

        if (present(success)) then
            if (res == 0) then
                success = .false.
            else
                success = .true.
            end if
        end if
    end subroutine

    !> @brief  Remove a value from the set.
    !!
    subroutine qset_remove(self, val_data, success)
        class(qset_t), intent(inout) :: self
        type(*), target, intent(in) :: val_data
        logical, optional, intent(out) :: success

        integer :: res

        res = set_remove_c(self%q_cp, c_loc(val_data))

        if (present(success)) then
            if (res == 0) then
                success = .false.
            else
                success = .true.
            end if
        end if
    end subroutine

    !> @brief  Remove a value from the set.
    !!
    subroutine qset_removestr(self, str_data, success)
        class(qset_t), intent(inout) :: self
        character(len=*), target, intent(in) :: str_data
        logical, optional, intent(out) :: success

        type(c_ptr) ::  cstr_new_p
        integer :: res

        call qlibc_copy_string_new_c(c_loc(str_data), cstr_new_p, len_trim(str_data,kind=c_size_t))

        res = set_remove_c(self%q_cp, cstr_new_p)
        call qlibc_free_c(cstr_new_p)

        if (present(success)) then
            if (res == 0) then
                success = .false.
            else
                success = .true.
            end if
        end if
    end subroutine

    !> @brief  Retrieve the number of entries in the set.
    !!
    function qset_size(self)
        integer :: qset_size
        class(qset_t), intent(in) :: self

        qset_size = qu_set_size_c(self%q_cp)
    end function

    !> @brief  Does the set has a particular value?
    !!
    function qset_has(self, val_data)
        logical :: qset_has
        class(qset_t), intent(in) :: self
        type(*), target, intent(in) :: val_data

        integer :: res

        res = set_query_c(self%q_cp, c_loc(val_data))
        if (res /= 0) then
            qset_has = .true.
        else
            qset_has = .false.
        end if
    end function

    !> @brief  Does the set has a string value?
    !!
    function qset_hasstr(self, str_data)
        logical :: qset_hasstr
        class(qset_t), intent(in) :: self
        character(len=*), target, intent(in) :: str_data

        type(c_ptr) :: cstr_new_p
        integer :: res

        call qlibc_copy_string_new_c(c_loc(str_data), cstr_new_p, len_trim(str_data,kind=c_size_t))

        res = set_query_c(self%q_cp, cstr_new_p)
        call qlibc_free_c(cstr_new_p)
        if (res /= 0) then
            qset_hasstr = .true.
        else
            qset_hasstr = .false.
        end if
    end function

    !> @brief  Initialize the object. Should be called before the use of the object.
    !!
    subroutine qset_objinit(obj)
        class(qset_obj_t), intent(inout) :: obj

        obj%wasInit = .true.
        obj%qobj = c_null_ptr
    end subroutine

    !> @brief  Get next element.
    !!
    function qset_getnext(self, obj)
        class(qset_t), intent(in) :: self
        class(qset_obj_t), intent(inout) :: obj
        logical :: qset_getnext

        if (.not.c_associated(obj%qobj)) then
            obj%qobj = qu_set_new_iterator_c()
            call set_iterate_c(self%q_cp, obj%qobj)
            obj%size_data = self%size_data
        end if

        if (set_iter_has_more_c(obj%qobj) /= 0) then
            obj%data_p = set_iter_next_c(obj%qobj);
            qset_getnext = .true.
        else
            qset_getnext = .false.
        end if
    end function

    !> @brief  Get data from the object.
    !!
    subroutine qset_getdata(obj, val_data)
        class(qset_obj_t), intent(in) :: obj
        type(*), target :: val_data

        logical(c_bool) :: freemem = .false.

        call qlibc_copy_data_c(obj%data_p, c_loc(val_data), obj%size_data, freemem)
    end subroutine

    !> @brief  Get string data from the object.
    !!
    subroutine qset_getdatastr(obj, str_data)
        class(qset_obj_t), intent(in) :: obj
        character(len=:), allocatable, target, intent(inout) :: str_data  !< returned string.

        logical(c_bool) :: freemem = .false.
        integer(c_size_t) :: str_len

        str_len = qlibc_cstr_len_c(obj%data_p)

        if (allocated(str_data)) then
            if (len(str_data) /= str_len) then
                deallocate(str_data)
                allocate(character(len=str_len) :: str_data)
            end if
        else
            allocate(character(len=str_len) :: str_data)
        end if
        call qlibc_copy_data_c(obj%data_p, c_loc(str_data), str_len, freemem)
    end subroutine

    !> @brief  Returns the array containing all the elements in this set.
    !!
    subroutine qset_toarray(self, val_arr)
        class(qset_t), intent(in) :: self
        type(*), dimension(*) :: val_arr   !< return array which is filled with values. This array should be large enough to contain all values.

        call qu_set_toarray_c(self%q_cp, val_arr, self%size_data)
    end subroutine

    !> @brief Returns the array containing all the elements (strings) in this set.
    !! Character length should be large enough to contain the longest string in the set.
    !!
    subroutine qset_toarraystr(self, str_arr)
        class(qset_t), intent(in) :: self
        character(len=*), dimension(:), target :: str_arr   !< return array which is filled with values.

        integer :: i, nval
        character(len=1,kind=c_char) :: cchar
        integer(c_size_t) :: str_size_B

        if (size(str_arr) == 0) return
        ! Fill strings with spaces.
        nval = qu_set_size_c(self%q_cp)
        do i = 1, nval
            str_arr(i) = " "
        end do

        str_size_B = int(storage_size(str_arr) / storage_size(cchar), kind=c_size_t)

        ! Fill strings with values.
        call qu_set_toarray_str_c(self%q_cp, c_loc(str_arr), str_size_B)
    end subroutine

    !> @brief Set copy procedure.
    !!
    subroutine qset_copy(to, from)
        type(qset_t), intent(inout) :: to
        type(qset_t), intent(in)    :: from

        call qset_free(to);

        if (.not. from%was_init) return

        if (.not. c_associated(from%q_cp))  return

        to%q_cp = set_new_c(from%fun_hash, from%fun_equal)
        call set_register_free_function_c(to%q_cp, c_funloc(qlibc_free_c))
        to%size_data     = from%size_data
        to%is_string_set = from%is_string_set

        call qu_set_copy_c(to%q_cp, from%q_cp, from%size_data, from%is_string_set)

    end subroutine

    !> @brief Set clear procedure.
    !!
    subroutine qset_clear(self)
        class(qset_t), intent(inout) :: self

        call qset_free(self)
        self%q_cp = set_new_c(self%fun_hash, self%fun_equal)
    end subroutine

    !> @brief  Destructor of the qset object.
    !!
    subroutine qset_objfree(obj)
        type(qset_obj_t), intent(in) :: obj

        call qlibc_free_c(obj%qobj)
    end subroutine

    !> @brief Copy procedure for qset_obj_t. Should not be called.
    !!
    subroutine qset_obj_copy(to, from)
        type(qset_obj_t), intent(inout) :: to
        type(qset_obj_t), intent(in)    :: from

        print *, "Error. qset_obj_t object can not be copied."
        print *, "Sub: qset_obj_copy"
        stop "Program terminated!"
    end subroutine

end module

