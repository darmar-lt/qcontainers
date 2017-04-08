#include "include_defines.fi"

!===========================================
!
!===========================================
module qhashtbl_util_m
    use iso_c_binding
    use qlibc_util_m
    implicit none

    interface
        type(c_ptr) function qhashtbl_getobj_c() bind(c)
            use iso_c_binding, only: c_ptr
        end function

        subroutine qhashtbl_objinit_c(obj) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
        end subroutine

        subroutine qhashtbl_getdata_c(obj, val_data) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
            type(c_ptr), value :: val_data
        end subroutine

        subroutine qhashtbl_getname_c(obj, name) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
            type(c_ptr), value :: name
        end subroutine

        integer(c_size_t) function qhashtbl_getnamesize_c(obj) bind(c)
            use iso_c_binding, only: c_ptr, c_size_t
            type(c_ptr), value :: obj
        end function
    end interface

    interface
        !void qhashtbl_copy_c(qhashtbl_t **to, qhashtbl_t *from)
        subroutine qhashtbl_copy_c(to, from) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr) :: to
            type(c_ptr), value :: from
        end subroutine
    end interface
end module qhashtbl_util_m

!===========================================
!
!===========================================
module qhashtbl_interface_m
    implicit none
    !*******************************************
    ! Interfaces for functions in qlist.h
    !*******************************************
    !

    interface
        !qhashtbl_t *qhashtbl(size_t range, int options);
        function qhashtbl_c(range, options) bind(c,name="qhashtbl")
            use iso_c_binding, only: c_ptr, c_size_t, c_int
            type(c_ptr) :: qhashtbl_c
            integer(c_size_t), value :: range
            integer(c_int), value :: options
        end function

        !bool qhashtbl_put(qhashtbl_t *tbl, const char *name, const void *data, size_t size);
        function qhashtbl_put_c(tbl, name, val_data, size) bind(c,name="qhashtbl_put")
            use iso_c_binding, only: c_bool, c_ptr, c_char, c_size_t
            logical(c_bool) :: qhashtbl_put_c
            type(c_ptr), value :: tbl
            character(kind=c_char), dimension(*) :: name
            type(c_ptr), value :: val_data
            integer(c_size_t), value :: size
        end function

        !void *qhashtbl_get(qhashtbl_t *tbl, const char *name, size_t *size, bool newmem);
        function qhashtbl_get_c(tbl, name, size, newmem) bind(c,name="qhashtbl_get")
            use iso_c_binding, only: c_ptr, c_char, c_bool
            type(c_ptr) :: qhashtbl_get_c
            type(c_ptr), value :: tbl
            character(kind=c_char), dimension(*) :: name
            type(c_ptr), value :: size
            logical(c_bool), value :: newmem
        end function

        !bool qhashtbl_remove(qhashtbl_t *tbl, const char *name);
        function qhashtbl_remove_c(tbl, name) bind(c,name="qhashtbl_remove")
            use iso_c_binding, only: c_bool, c_ptr, c_char
            logical(c_bool) :: qhashtbl_remove_c
            type(c_ptr), value :: tbl
            character(kind=c_char), dimension(*) :: name
        end function

        !bool qhashtbl_getnext(qhashtbl_t *tbl, qhashtbl_obj_t *obj, bool newmem);
        function qhashtbl_getnext_c(tbl, obj, newmem) bind(c,name="qhashtbl_getnext")
            use iso_c_binding, only: c_bool, c_ptr
            logical(c_bool) :: qhashtbl_getnext_c
            type(c_ptr), value :: tbl
            type(c_ptr), value :: obj
            logical(c_bool), value :: newmem
        end function

        !size_t qhashtbl_size(qhashtbl_t *tbl);
        function qhashtbl_size_c(tbl) bind(c,name="qhashtbl_size")
            use iso_c_binding, only: c_size_t, c_ptr
            integer(c_size_t) :: qhashtbl_size_c
            type(c_ptr), value :: tbl
        end function

        !void qhashtbl_clear(qhashtbl_t *tbl);
        subroutine qhashtbl_clear_c(tbl) bind(c,name="qhashtbl_clear")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: tbl
        end subroutine

        !void qhashtbl_free(qhashtbl_t *tbl);
        subroutine qhashtbl_free_c(tbl) bind(c, name="qhashtbl_free")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: tbl
        end subroutine

    end interface
end module qhashtbl_interface_m

!===========================================
!
!===========================================
module qhashtbl_m
    use iso_c_binding
    use qhashtbl_interface_m
    use qlibc_util_m
    use qhashtbl_util_m
    implicit none


    type, public :: qhashtbl_t
        private
        type(c_ptr) :: q_cp = c_null_ptr
        integer(c_size_t) :: size_data

        logical :: was_init = .false.
        ! It seems, that gfortran has a bug, which prevents qobj to be initialized to c_null_ptr here.
        ! variable wasInit is used as a workaround.
    contains
        final :: qhashtbl_free
        procedure :: new => qhashtbl_new          !< Constructor of the container. Should be called before the use.
        procedure :: put => qhashtbl_put          !< Put key-value pair to the container.
        procedure :: putstr => qhashtbl_putstr    !< Put string to the container.
        procedure :: get => qhashtbl_get          !< Get data.
        procedure :: getstr => qhashtbl_getstr    !< Get character data.
        procedure :: haskey => qhashtbl_haskey    !< Return if the key is in the container.
        procedure :: remove => qhashtbl_remove    !< Remove key from the container.
        procedure :: size => qhashtbl_size        !< Get current size (number of pairs) in the container.
        procedure :: getnext => qhashtbl_getnext  !< Get next object.
        procedure :: clear => qhashtbl_clear      !< Remove all values from the container.
    end type

    ! Type describes a one object of the qhashtbl
    type, public :: qhashtbl_obj_t
        type(c_ptr) :: qobj = c_null_ptr
        logical :: wasInit = .false.
        ! It seems, that gfortran has a bug, which prevents qobj to be initialized to c_null_ptr here.
        ! Variable wasInit is used as a workaround.
    contains
        final :: qhashtbl_objfree
        procedure :: getdata => qhashtbl_getdata                !< Get data from the object.
        procedure :: getname => qhashtbl_getname                !< Get name from the object.
        procedure :: init => qhashtbl_objinit                   !< Initialize the object.
    end type

    interface assignment(=)
        module procedure qhashtbl_copy
        module procedure qhashtbl_obj_copy
    end interface

    private
    public :: assignment(=)

contains

    !> @brief qhashtbl constructor
    !!
    subroutine qhashtbl_new(self, range, size_data)
        class(qhashtbl_t) :: self
        integer, optional, intent(in) :: range      !< initial size of index range. Value of 0 will use default value.
        integer, optional, intent(in) :: size_data  !< size of each element in bytes.

        integer(c_int) :: opt = 0
        integer(c_size_t) :: ran

        if (present(range)) then
            ran = range
        else
            ran = 0
        end if
        self%q_cp = qhashtbl_c(ran, opt)
        if (present(size_data)) then
            self%size_data = int(size_data, kind=c_size_t)
        else
            self%size_data = 0
        end if

        self%was_init = .true.
    end subroutine

    !> @brief  Destructor of the qhashtbl.
    !!
    subroutine qhashtbl_free(self)
        type(qhashtbl_t), intent(inout) :: self

        if (.not. self%was_init) then
            self%q_cp = c_null_ptr
            self%was_init = .true.
        end if

        if (c_associated(self%q_cp)) then
            call qhashtbl_free_c(self%q_cp)
            self%q_cp = c_null_ptr
        end if
    end subroutine

    !> @brief  Put a value into this table.
    !!
    subroutine qhashtbl_put(self, name, val_data, size_data, success)
        class(qhashtbl_t), intent(inout) :: self
        character(len=*), intent(in) :: name       !< key name.
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(in) :: val_data   !< data to put
#else
#include "nocheck_val_data.fi"
#endif
        integer, optional, intent(in)  :: size_data !< size of data object.
        logical, optional, intent(out) :: success  !< true if successful.

        integer(c_size_t) :: sd
        logical(c_bool) :: suc_c

        if (present(size_data)) then
            sd = int(size_data, kind=c_size_t)
        else
            sd = self%size_data
        end if

        suc_c = qhashtbl_put_c(self%q_cp, f_c_string(name), c_loc(val_data), sd)
        if (present(success)) success = suc_c
    end subroutine

    !> @brief  Put a string into this table.
    !!
    subroutine qhashtbl_putstr(self, name, str_data, success)
        class(qhashtbl_t), intent(inout) :: self
        character(len=*), intent(in)     :: name      !< key name.
        character(len=*), target, intent(in) :: str_data  !< string to put.
        logical, optional, intent(out)   :: success   !< true if successful.

        logical(c_bool) :: suc_c

        suc_c = qhashtbl_put_c(self%q_cp, f_c_string(name), c_loc(str_data), len(str_data, kind=c_size_t))
        if (present(success)) success = suc_c
    end subroutine

    !> @brief Get data from the table.
    !!
    subroutine qhashtbl_get(self, name, val_data, found)
        class(qhashtbl_t), intent(in) :: self
        character(len=*), intent(in)  :: name  !< key name.
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target :: val_data   !< returned data.
#else
#include "nocheck_val_data.fi"
#endif
        logical, intent(out) :: found          !< true if key was found.

        integer(c_size_t), target :: size_data
        type(c_ptr) :: size_data_p
        type(c_ptr) :: val_data_p
        logical(c_bool) :: newmem = .false.

        size_data_p = c_loc(size_data)
        val_data_p = qhashtbl_get_c(self%q_cp, f_c_string(name), size_data_p, newmem)
        if (c_associated(val_data_p)) then
            call qlibc_copy_data_c(val_data_p, c_loc(val_data), size_data, newmem)
            found = .true.
        else
            found = .false.
        end if
    end subroutine

    !> @brief Get character data.
    !!
    subroutine qhashtbl_getstr(self, name, str_data, found)
        class(qhashtbl_t), intent(in) :: self
        character(len=*), intent(in)  :: name  !< key name.
        character(len=:), allocatable, target, intent(inout) :: str_data  !< returned string.
        logical, intent(out) :: found          !< true if key was found, else false.

        integer(c_size_t), target :: size_data
        type(c_ptr) :: size_data_p
        type(c_ptr) :: val_data_p
        logical(c_bool) :: newmem = .false.

        size_data_p = c_loc(size_data)
        val_data_p = qhashtbl_get_c(self%q_cp, f_c_string(name), size_data_p, newmem)
        if (c_associated(val_data_p)) then
            if (allocated(str_data)) then
                if (len(str_data) /= size_data) then
                    deallocate(str_data)
                    allocate(character(len=size_data) :: str_data)
                end if
            else
                allocate(character(len=size_data) :: str_data)
            end if
            call qlibc_copy_data_c(val_data_p, c_loc(str_data), size_data, newmem)
            found = .true.
        else
            found = .false.
        end if
    end subroutine

    !> @brief Return if the key is in the container.
    !!
    logical function qhashtbl_haskey(self, name)
        class(qhashtbl_t), intent(in) :: self
        character(len=*), intent(in)  :: name  !< key name.

        type(c_ptr) :: size_data_p
        type(c_ptr) :: val_data_p
        logical(c_bool) :: newmem = .false.

        size_data_p = c_null_ptr
        val_data_p = qhashtbl_get_c(self%q_cp, f_c_string(name), size_data_p, newmem)
        if (c_associated(val_data_p)) then
            qhashtbl_haskey = .true.
        else
            qhashtbl_haskey = .false.
        end if
    end function

    !> @brief Remove an object from this table.
    !!
    subroutine qhashtbl_remove(self, name, success)
        class(qhashtbl_t), intent(inout) :: self
        character(len=*), intent(in)  :: name     !< key name.
        logical, optional, intent(out) :: success !< true if successful, else false.

        logical(c_bool) :: suc

        suc = qhashtbl_remove_c(self%q_cp, f_c_string(name))
        if (present(success)) success = suc
    end subroutine

    !> @brief Get next element.
    !!
    logical function qhashtbl_getnext(self, obj)
        class(qhashtbl_t), intent(in) :: self
        type(qhashtbl_obj_t), intent(inout) :: obj  !< qhashtbl object.
        logical(c_bool) :: newmem = .false.

        qhashtbl_getnext = qhashtbl_getnext_c(self%q_cp, obj%qobj, newmem)
    end function

    !> @brief Returns the number of keys in this hashtable.
    !!
    integer function qhashtbl_size(self)
        class(qhashtbl_t), intent(in) :: self

        qhashtbl_size = qhashtbl_size_c(self%q_cp)
    end function

    !> @brief Clears this hashtable so that it contains no keys.
    !!
    subroutine qhashtbl_clear(self)
        class(qhashtbl_t), intent(inout) :: self

        call qhashtbl_clear_c(self%q_cp)
    end subroutine

    !> @brief Copy procedure.
    !!
    subroutine qhashtbl_copy(to, from)
        type(qhashtbl_t), intent(inout) :: to
        type(qhashtbl_t), intent(in)    :: from

        if (.not. to%was_init) then
            to%q_cp = c_null_ptr
            to%was_init = .true.
        end if
        call qhashtbl_copy_c(to%q_cp, from%q_cp)
        to%size_data = from%size_data
    end subroutine



    !**********************************************
    !*** Procedures for work with qhashtbl_obj_t
    !**********************************************

    !> @brief Initialize the object. Should be called before the use of the object.
    !!
    subroutine qhashtbl_objinit(obj)
        class(qhashtbl_obj_t), intent(inout) :: obj

        if (.not. obj%wasInit) then
            obj%wasInit = .true.
            obj%qobj = c_null_ptr
        end if

        if ( .not. c_associated(obj%qobj)) then
            obj%qobj = qhashtbl_getobj_c()
        else
            call qhashtbl_objinit_c(obj%qobj)
        end if
    end subroutine

    !> @brief Destructor.
    !!
    subroutine qhashtbl_objfree(obj)
        type(qhashtbl_obj_t), intent(inout) :: obj

        if (.not. obj%wasInit) then
            obj%qobj = c_null_ptr
            obj%wasInit = .true.
        end if
        if (c_associated(obj%qobj)) then
            call qlibc_free_c(obj%qobj)
            obj%qobj = c_null_ptr
        end if
    end subroutine

    !> @brief Copy procedure. Should not be called.
    !!
    subroutine qhashtbl_obj_copy(to, from)
        type(qhashtbl_obj_t), intent(inout) :: to
        type(qhashtbl_obj_t), intent(in)    :: from

        print *, "Error. qhashtbl_obj_t object can not be copied."
        print *, "Sub: qhashtbl_obj_copy"
        stop "Program terminated!"
    end subroutine

    !> @brief Get data from the object.
    !!
    subroutine qhashtbl_getdata(obj, val_data)
        class(qhashtbl_obj_t) :: obj
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target :: val_data   !< returned data.
#else
#include "nocheck_val_data.fi"
#endif

        call qhashtbl_getdata_c(obj%qobj, c_loc(val_data))
    end subroutine

    !> @brief Get name from the object.
    !!
    subroutine qhashtbl_getname(obj, name)
        class(qhashtbl_obj_t) :: obj
        character(len=:), allocatable, target, intent(inout) :: name  !< returned name

        integer(c_size_t) :: namesize

        namesize = qhashtbl_getnamesize_c(obj%qobj)
        if (allocated(name)) then
            if (len(name) /= namesize) then
                deallocate(name)
                allocate(character(len=namesize) :: name)
            end if
        else
            allocate(character(len=namesize) :: name)
        end if

        call qhashtbl_getname_c(obj%qobj, c_loc(name))
    end subroutine

end module


#ifdef USE_COMPILER_DIRECTIVE
#undef USE_COMPILER_DIRECTIVE
#endif
