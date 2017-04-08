
#include "include_defines.fi"

module qlist_util_m

    !*******************************************
    ! Interfaces for functions in qlibc_util.c
    !*******************************************
    !
    interface
        ! bool qlist_debug(qlist_t *list, FILE *out);
        logical(c_bool) function qlist_debug_c(list) bind(c)
            use iso_c_binding, only: c_ptr, c_bool
            type(c_ptr), value :: list
        end function

        ! void qlist_getat_c(qlist_t *list, int index, void *val_data, bool *suc)
        subroutine qlist_getat_c(list, idx, val_data, suc) bind(c)
            use iso_c_binding, only: c_ptr, c_int, c_bool
            type(c_ptr), value :: list
            integer(c_int), value :: idx
#ifndef USE_COMPILER_DIRECTIVE
            type(*) :: val_data
#else
#include "nocheck_val_data.fi"
#endif
            logical(c_bool) :: suc
        end subroutine

        ! void qlist_popat_c(qlist_t *list, int index, void *val_data, bool *suc)
        subroutine qlist_popat_c(list, idx, val_data, suc) bind(c)
            use iso_c_binding, only: c_ptr, c_int, c_bool
            type(c_ptr), value :: list
            integer(c_int), value :: idx
#ifndef USE_COMPILER_DIRECTIVE
            type(*) :: val_data
#else
#include "nocheck_val_data.fi"
#endif
            logical(c_bool) :: suc
        end subroutine

        ! void qlist_toarray_c(qlist_t *list, void *val_array);
        subroutine qlist_toarray_c(list, val_data) bind(c)
            use iso_c_binding, only: c_ptr, c_int
            type(c_ptr), value  :: list
#ifndef USE_COMPILER_DIRECTIVE
            type(*), dimension(*) :: val_data
#else
#include "nocheck_val_data.fi"
#endif
        end subroutine
    end interface

    interface
        !void qlist_copy_c(qhashtbl_t **to, qhashtbl_t *from)
        subroutine qlist_copy_c(to, from) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr) :: to
            type(c_ptr), value :: from
        end subroutine
    end interface

endmodule qlist_util_m

!===========================================
!
!===========================================
module qlist_obj_m
    use iso_c_binding
    use qlibc_util_m
    !use qlist_interfaces_m
    implicit none

    interface
        type(c_ptr) function qlist_getobj_c() bind(c)
            import :: c_ptr
        end function

        subroutine qlist_objinit_c(obj) bind(c)
            import :: c_ptr
            type(c_ptr), value :: obj
        end subroutine

        subroutine qlist_objfree_c(obj) bind(c)
            import :: c_ptr
            type(c_ptr), value :: obj
        end subroutine

        subroutine qlist_getdata_c(obj, val_data) bind(c)
            import :: c_ptr
            type(c_ptr), value :: obj
#ifndef USE_COMPILER_DIRECTIVE
            type(*) :: val_data
#else
#include "nocheck_val_data.fi"
#endif
        end subroutine
    end interface

endmodule qlist_obj_m

!===========================================
!
!===========================================
module qlist_interfaces_m
    implicit none
    !*******************************************
    ! Interfaces for functions in qlist.h
    !*******************************************
    !
    interface
        !qlist_t *qlist(int options); /*!< qlist constructor */
        function qlist_c(options) bind(c, name="qlist")
            use iso_c_binding
            integer(c_int), value :: options
            type(c_ptr) :: qlist_c
        end function

        ! size_t qlist_setsize(qlist_t *list, size_t max);
        integer(c_size_t) function qlist_setsize_c(list, maxsize) bind(c, name="qlist_setsize")
            use iso_c_binding
            type(c_ptr), value :: list
            integer(c_size_t), value :: maxsize
        end function

        ! bool qlist_addfirst(qlist_t *list, const void *data, size_t size);
        logical(c_bool) function qlist_addfirst_c(list, val_data, size_data) bind(c, name="qlist_addfirst")
            use iso_c_binding, only: c_ptr, c_size_t, c_bool
            type(c_ptr), value       :: list
#ifndef USE_COMPILER_DIRECTIVE
            type(*) :: val_data
#else
#include "nocheck_val_data.fi"
#endif
            integer(c_size_t), value :: size_data
        end function

        ! bool qlist_addlast(qlist_t *list, const void *data, size_t size);
        logical(c_bool) function qlist_addlast_c(list, val_data, size_data) bind(c, name="qlist_addlast")
            use iso_c_binding, only: c_ptr, c_size_t, c_bool
            type(c_ptr), value       :: list
#ifndef USE_COMPILER_DIRECTIVE
            type(*) :: val_data
#else
#include "nocheck_val_data.fi"
#endif
            integer(c_size_t), value :: size_data
        end function

        !extern bool qlist_addat(qlist_t *list, int index, const void *data, size_t size);
        logical(c_bool) function qlist_addat_c(list, index, val_data, size_data) bind(c, name="qlist_addat")
            use iso_c_binding, only: c_ptr, c_size_t, c_bool, c_int
            type(c_ptr), value       :: list
            integer(c_int), value    :: index
#ifndef USE_COMPILER_DIRECTIVE
            type(*) :: val_data
#else
#include "nocheck_val_data.fi"
#endif
            integer(c_size_t), value :: size_data
        end function

        ! size_t qlist_size(qlist_t *list);
        integer(c_size_t) function qlist_size_c(list) bind(c, name="qlist_size")
            use iso_c_binding, only: c_ptr, c_size_t
            type(c_ptr), value :: list
        end function

        ! bool qlist_getnext(qlist_t *list, qlist_obj_t *obj, bool newmem);
        logical(c_bool) function qlist_getnext_c(list, obj, newmem) bind(c, name="qlist_getnext")
            use iso_c_binding !, only: c_ptr, c_bool
            type(c_ptr), value :: list
            type(c_ptr), value :: obj
            logical(c_bool), value :: newmem
        end function

        ! bool qlist_removefirst(qlist_t *list);
        logical(c_bool) function qlist_removefirst_c(list) bind(c, name="qlist_removefirst")
            use iso_c_binding, only: c_ptr, c_bool
            type(c_ptr), value :: list
        end function

        ! bool qlist_removelast(qlist_t *list);
        logical(c_bool) function qlist_removelast_c(list) bind(c, name="qlist_removelast")
            use iso_c_binding, only: c_ptr, c_bool
            type(c_ptr), value :: list
        end function

        ! bool qlist_removeat(qlist_t *list, int index);
        logical(c_bool) function qlist_removeat_c(list, idx) bind(c, name="qlist_removeat")
            use iso_c_binding, only: c_ptr, c_int, c_bool
            type(c_ptr), value    :: list
            integer(c_int), value :: idx
        end function

        ! size_t qlist_datasize(qlist_t *list);
        integer(c_size_t) function qlist_datasize_c(list) bind(c, name="qlist_datasize")
            use iso_c_binding, only: c_ptr, c_size_t
            type(c_ptr), value :: list
        end function

        ! void qlist_reverse(qlist_t *list);
        subroutine qlist_reverse_c(list) bind(c, name="qlist_reverse")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: list
        end subroutine

        ! void qlist_clear(qlist_t *list);
        subroutine qlist_clear_c(list) bind(c, name="qlist_clear")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: list
        end subroutine

        ! void qlist_free(qlist_t *list);
        subroutine qlist_free_c(list) bind(c, name="qlist_free")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: list
        end subroutine

    end interface

endmodule qlist_interfaces_m

!===========================================
!
!===========================================
module qlist_m
    use iso_c_binding
    use qlist_util_m
    use qlist_obj_m
    use qlist_interfaces_m
    implicit none


    type, public :: qlist_t
        private
        type(c_ptr) :: q_cp = c_null_ptr
        integer(c_size_t) :: size_data

        logical :: was_init = .false.
    contains
        final :: qlist_free
        procedure :: new => qlist_new                  !< Constructor of the container.
        procedure :: setsize => qlist_setsize          !< Limit maximum number of elements allowed in this list. Default is unlimited.
        procedure :: addfirst => qlist_addfirst        !< Inserts an element at the beginning of this list.
        procedure :: addlast => qlist_addlast          !< Appends an element to the end of this list.
        procedure :: addat => qlist_addat              !< Inserts an element at the specified position in this list.
        procedure :: size => qlist_size                !< Returns the number of elements in this list.
        procedure :: getnext => qlist_getnext          !< Get next element in this list.
        procedure :: getfirst => qlist_getfirst        !< Returns the first element in this list.
        procedure :: getlast => qlist_getlast          !< Returns the last element in this list.
        procedure :: getat => qlist_getat              !< Returns the element at the specified position.
        procedure :: popfirst => qlist_popfirst        !< Returns and removes the first element in this list.
        procedure :: poplast => qlist_poplast          !< Returns and removes the last element in this list.
        procedure :: popat => qlist_popat              !< Returns and removes the element at the specified position.
        procedure :: removefirst => qlist_removefirst  !< Removes the first element in this list.
        procedure :: removelast => qlist_removelast    !< Removes the last element in this list.
        procedure :: removeat => qlist_removeat        !< Removes the element at the specified position in this list.
        procedure :: datasize => qlist_datasize        !< Returns the sum of element sizes.
        procedure :: reverse => qlist_reverse          !< Reverse the order of elements.
        procedure :: clear => qlist_clear              !< Removes all of the elements from this list.
        procedure :: toarray => qlist_toarray          !< Returns the array containing all the elements in this list.
    end type


    type, public :: qlist_obj_t
        private
        type(c_ptr) :: qobj = c_null_ptr
        logical :: wasInit = .false.
    contains
        final :: qlist_objfree
        procedure :: init => qlist_objinit
        procedure :: getdata => qlist_getdata
    end type

    interface assignment(=)
        module procedure qlist_copy
        module procedure qlist_obj_copy
    end interface

    private
    public :: assignment(=)


contains

    !> @brief qlist constructor
    !!
    subroutine qlist_new(self, size_data)
        class(qlist_t) :: self
        integer, optional, intent(in) :: size_data !< size of each element in bytes

        integer(c_int) :: opt

        opt = 0
        self%q_cp = qlist_c(opt)
        if (present(size_data)) then
            self%size_data = int(size_data, kind=c_size_t)
        else
            self%size_data = 0
        end if

        self%was_init = .true.
    end subroutine

    !>
    !!  @brief Limit maximum number of elements allowed in this list.
    !!         The default maximum number of elements is unlimited.
    !!
    subroutine qlist_setsize(self, maxsize, maxsize_old)
        class(qlist_t), intent(inout)  :: self
        integer, intent(in)            :: maxsize      !< maximum number of elements. 0 means no limit
        integer, optional, intent(out) :: maxsize_old  !< previous maximum number

        integer(c_size_t) :: mso

        mso = qlist_setsize_c(self%q_cp, int(maxsize, kind=c_size_t))
        if (present(maxsize_old)) &
            maxsize_old = int(mso)
    end subroutine

    !> @brief  Inserts an element at the beginning of this list.
    !!
    subroutine qlist_addfirst(self, val_data, size_data, success)
        class(qlist_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< data to add
#else
#include "nocheck_val_data.fi"
#endif
        integer, optional, intent(in)  :: size_data  !< size of data object
        logical, optional, intent(out) :: success    !< true if successful

        integer(c_size_t) :: sd
        logical(c_bool) :: suc_c

        if (present(size_data)) then
            sd = int(size_data, kind=c_size_t)
        else
            sd = self%size_data
        end if
        suc_c = qlist_addfirst_c(self%q_cp, val_data, sd)
        if (present(success)) success = suc_c
    end subroutine

    !> @brief  Appends an element to the end of this list.
    !!
    subroutine qlist_addlast(self, val_data, size_data, success)
        class(qlist_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< data to add
#else
#include "nocheck_val_data.fi"
#endif
        integer, optional, intent(in)  :: size_data  !< size of data object
        logical, optional, intent(out) :: success    !< true if successful

        integer(c_size_t) :: sd
        logical(c_bool) :: suc_c

        if (present(size_data)) then
            sd = int(size_data, kind=c_size_t)
        else
            sd = self%size_data
        end if
        suc_c = qlist_addlast_c(self%q_cp, val_data, sd)
        if (present(success)) success = suc_c
    end subroutine

    !> @brief  Inserts an element at the specified position in this list.
    !!
    subroutine qlist_addat(self, idx, val_data, size_data, success)
        class(qlist_t), intent(inout) :: self
        integer, intent(in)           :: idx        !< at which index to insert (1-size). If idx<0, then idx=(size+idx+1).
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< data to add
#else
#include "nocheck_val_data.fi"
#endif
        integer, optional, intent(in)  :: size_data  !< size of data object
        logical, optional, intent(out) :: success    !< true if successful

        integer(c_size_t) :: sd
        integer(c_int)    :: idx_c
        logical(c_bool) :: suc_c

        if (present(size_data)) then
            sd = int(size_data, kind=c_size_t)
        else
            sd = self%size_data
        end if

        if (idx > 0) then
            idx_c = int(idx - 1, kind=c_int)
        else if (idx < 0) then
            idx_c = int(idx, kind=c_int)
        else
            ! idx == 0
            if (present(success)) success = .false.
            return
        end if
        suc_c = qlist_addat_c(self%q_cp, idx_c, val_data, sd)
        if (present(success)) success = suc_c
    end subroutine

    subroutine qlist_debug(self)
        class(qlist_t), intent(inout) :: self
        logical(c_bool) :: iserr_c

        iserr_c = qlist_debug_c(self%q_cp)
    end subroutine

    !> @brief  Returns the number of elements in this list.
    !!
    integer function qlist_size(self)
        class(qlist_t), intent(in) :: self

        qlist_size = int(qlist_size_c(self%q_cp))
    end function

    !> @brief  Get next element in this list.
    !!
    !! @return true if found otherwise returns false
    !!
    logical function qlist_getnext(self, obj)
        class(qlist_t), intent(in) :: self
        type(qlist_obj_t), intent(inout) :: obj  !< found data will be stored in this structure. Should be initialized before using it.
        logical(c_bool) :: newmem = .false.

        qlist_getnext = qlist_getnext_c(self%q_cp, obj%qobj, newmem)
    end function

    !> @brief  Returns the first element in this list.
    !!
    subroutine qlist_getfirst(self, val_data, success)
        class(qlist_t), intent(in) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< returned value
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: suc

        call qlist_getat_c(self%q_cp, 0_c_int, val_data, suc)
        if (present(success)) success = suc
    end subroutine

    !> @brief  Returns the last element in this list.
    !!
    subroutine qlist_getlast(self, val_data, success)
        class(qlist_t), intent(in) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< returned value
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: suc

        call qlist_getat_c(self%q_cp, -1_c_int, val_data, suc)
        if (present(success)) success = suc
    end subroutine

    !> @brief  Returns the element at the specified position.
    !!
    subroutine qlist_getat(self, idx, val_data, success)
        class(qlist_t), intent(in) :: self
        integer, intent(in)        :: idx          !< index at which the element should be returned (1-size). If idx<0, then idx=(size+idx+1)
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< returned value
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: suc
        integer(c_int)  :: idx_c

        if (idx > 0) then
            idx_c = int(idx - 1, kind=c_int)
        else if (idx < 0) then
            idx_c = int(idx, kind=c_int)
        else
            ! idx == 0
            if (present(success)) success = .false.
            return
        end if

        call qlist_getat_c(self%q_cp, idx_c, val_data, suc)
        if (present(success)) success = suc
    end subroutine

    !> @brief  Returns and removes the first element in this list.
    !!
    subroutine qlist_popfirst(self, val_data, success)
        class(qlist_t), intent(in) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< returned value
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: suc

        call qlist_popat_c(self%q_cp, 0_c_int, val_data, suc)
        if (present(success)) success = suc
    end subroutine

    !> @brief  Returns and removes the last element in this list.
    !!
    subroutine qlist_poplast(self, val_data, success)
        class(qlist_t), intent(in) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< returned value
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: suc

        call qlist_popat_c(self%q_cp, -1_c_int, val_data, suc)
        if (present(success)) success = suc
    end subroutine

    !> @brief  Returns and removes the element at the specified position.
    !!
    subroutine qlist_popat(self, idx, val_data, success)
        class(qlist_t), intent(in) :: self
        integer, intent(in)        :: idx          !< index at which the element should be returned and removed
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< returned value
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: suc

        call qlist_popat_c(self%q_cp, int(idx, kind=c_int), val_data, suc)
        if (present(success)) success = suc
    end subroutine

    !> @brief  Removes the first element in this list.
    !!
    subroutine qlist_removefirst(self, success)
        class(qlist_t), intent(in) :: self
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: suc

        suc = qlist_removefirst_c(self%q_cp)
        if (present(success)) success = suc
    end subroutine

    !> @brief  Removes the last element in this list.
    !!
    subroutine qlist_removelast(self, success)
        class(qlist_t), intent(in) :: self
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: suc

        suc = qlist_removelast_c(self%q_cp)
        if (present(success)) success = suc
    end subroutine

    !> @brief  Removes the element at the specified position in this list.
    !!
    subroutine qlist_removeat(self, idx, success)
        class(qlist_t), intent(in) :: self
        integer, intent(in)        :: idx          !< index (0-size) at which the specified element is to be removed.
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: suc
        integer(c_int)  :: idx_c

        if (idx > 0) then
            idx_c = int(idx - 1, kind=c_int)
        else if (idx < 0) then
            idx_c = int(idx, kind=c_int)
        else
            ! idx == 0
            if (present(success)) success = .false.
            return
        end if

        suc = qlist_removeat_c(self%q_cp, idx_c)
        if (present(success)) success = suc
    end subroutine

    !> @brief  Returns the sum of element sizes.
    !!
    integer function qlist_datasize(self)
        class(qlist_t), intent(in) :: self

        qlist_datasize = int(qlist_datasize_c(self%q_cp))
    end function

    !> @brief  Reverse the order of elements.
    !!
    subroutine qlist_reverse(self)
        class(qlist_t), intent(inout) :: self

        call qlist_reverse_c(self%q_cp)
    end subroutine

    !> @brief  Removes all of the elements from this list.
    !!
    subroutine qlist_clear(self)
        class(qlist_t), intent(inout) :: self

        call qlist_clear_c(self%q_cp)
    end subroutine

    !> @brief  Destructor of the list.
    !!
    subroutine qlist_free(self)
        type(qlist_t), intent(inout) :: self

        if (.not. self%was_init) then
            self%q_cp = c_null_ptr
            self%was_init = .true.
        end if

        if (c_associated(self%q_cp)) then
            call qlist_free_c(self%q_cp)
            self%q_cp = c_null_ptr
        end if
    end subroutine

    !> @brief  Returns the array containing all the elements in this list.
    !!
    subroutine qlist_toarray(self, val_data)
        class(qlist_t), intent(in) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), dimension(*) :: val_data   !< return array which is filled with values. This array should be large enough to contain all values.
#else
#include "nocheck_val_data.fi"
#endif

        call qlist_toarray_c(self%q_cp, val_data)
    end subroutine

    !> @brief Copy procedure.
    !!
    subroutine qlist_copy(to, from)
        type(qlist_t), intent(inout) :: to
        type(qlist_t), intent(in)    :: from

        if (.not. to%was_init) then
            to%q_cp = c_null_ptr
            to%was_init = .true.
        end if

        call qlist_copy_c(to%q_cp, from%q_cp)
        to%size_data = from%size_data
    end subroutine


    !******************************************************
    !
    ! Procedures to work with qlist_obj_t
    !
    !******************************************************

    !> @brief  Initialize qlist object.
    !!
    subroutine qlist_objinit(obj)
        class(qlist_obj_t), intent(inout) :: obj

        if (.not. obj%wasInit) then
            obj%wasInit = .true.
            obj%qobj = c_null_ptr
        end if

        if ( .not. c_associated(obj%qobj)) then
            obj%qobj = qlist_getobj_c()
        else
            call qlist_objinit_c(obj%qobj)
        end if
    end subroutine

    !> @brief  Destructor of qlist object
    !!
    subroutine qlist_objfree(obj)
        type(qlist_obj_t), intent(inout) :: obj

        if (.not. obj%wasInit) then
            obj%qobj = c_null_ptr
            obj%wasInit = .true.
        end if
        if (c_associated(obj%qobj)) then
            call qlibc_free_c(obj%qobj)
            obj%qobj = c_null_ptr
        end if
    end subroutine

    !> @brief  Copy one object to another. Should not be called.
    !!
    subroutine qlist_obj_copy(to, from)
        type(qlist_obj_t), intent(inout) :: to
        type(qlist_obj_t), intent(in)    :: from

        print *, "Error. qlist_obj_t object can not be copied."
        print *, "Sub: qlist_obj_copy"
        stop "Program terminated!"

    end subroutine

    !> @brief  Returns the value in this object.
    !!
    subroutine qlist_getdata(obj, val_data)
        class(qlist_obj_t) :: obj
#ifndef USE_COMPILER_DIRECTIVE
        type(*) :: val_data   !< return value
#else
#include "nocheck_val_data.fi"
#endif

        call qlist_getdata_c(obj%qobj, val_data)
    end subroutine

end module qlist_m

#ifdef USE_COMPILER_DIRECTIVE
#undef USE_COMPILER_DIRECTIVE
#endif
