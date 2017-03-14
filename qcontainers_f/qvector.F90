#include "include_defines.fi"

!===========================================
!
!===========================================
module qvector_util_m
    use qlibc_util_m
    implicit none

    interface
        type(c_ptr) function qvector_getobj_c() bind(c)
            use iso_c_binding, only: c_ptr
        end function

        subroutine qvector_objinit_c(obj) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
        end subroutine

        ! void qvector_getdata_c(qvector_obj_t *obj, void *data, size_t objsize)
        subroutine qvector_getdata_c(obj, data, objsize) bind(c)
            use iso_c_binding, only: c_ptr, c_size_t
            type(c_ptr), value :: obj
            type(c_ptr), value :: data
            integer(c_size_t), value :: objsize
        end subroutine
    end interface

    interface
        !void qvector_toarray_c(qvector_t *vector, void *array) {
        subroutine qvector_toarray_c(vector, array) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: vector
            type(c_ptr), value :: array
        end subroutine
    end interface

end module qvector_util_m

module qvector_interfaces_m
    implicit none

    interface
        !qvector_t *qvector(size_t max, size_t objsize, int options);
        function qvector_c(max, objsize, options) bind(c, name="qvector")
            use iso_c_binding, only: c_ptr, c_size_t, c_int
            type(c_ptr) :: qvector_c
            integer(c_size_t), value :: max
            integer(c_size_t), value :: objsize
            integer(c_int), value :: options
        end function

        !bool qvector_addfirst(qvector_t *vector, const void *data);
        function qvector_addfirst_c(vector, data) bind(c, name="qvector_addfirst")
            use iso_c_binding, only: c_bool, c_ptr
            logical(c_bool) :: qvector_addfirst_c
            type(c_ptr), value :: vector
            type(c_ptr), value :: data
        end function

        !bool qvector_addlast(qvector_t *vector, const void *data);
        function qvector_addlast_c(vector, data) bind(c, name="qvector_addlast")
            use iso_c_binding, only: c_bool, c_ptr
            logical(c_bool) :: qvector_addlast_c
            type(c_ptr), value :: vector
            type(c_ptr), value :: data
        end function

        !bool qvector_addat(qvector_t *vector, int index, const void *data);
        function qvector_addat_c(vector, index, data) bind(c, name="qvector_addat")
            use iso_c_binding, only: c_bool, c_ptr, c_int
            logical(c_bool) :: qvector_addat_c
            type(c_ptr), value :: vector
            integer(c_int), value :: index
            type(c_ptr), value :: data
        end function

!        !void *qvector_getfirst(qvector_t *vector, bool newmem);
!        function qvector_getfirst_c(vector, newmem) bind(c, name="qvector_getfirst")
!            use iso_c_binding, only: c_ptr, c_bool
!            type(c_ptr) :: qvector_getfirst_c
!            type(c_ptr), value :: vector
!            logical(c_bool), value :: newmem
!        end function
!
!        !void *qvector_getlast(qvector_t *vector, bool newmem);
!        function qvector_getlast_c(vector, newmem) bind(c, name="qvector_getlast")
!            use iso_c_binding, only: c_ptr, c_bool
!            type(c_ptr) :: qvector_getlast_c
!            type(c_ptr), value :: vector
!            logical(c_bool), value :: newmem
!        end function

        !void *qvector_getat(qvector_t *vector, int index, bool newmem);
        function qvector_getat_c(vector, index, newmem) bind(c, name="qvector_getat")
            use iso_c_binding, only: c_ptr, c_int, c_bool
            type(c_ptr) :: qvector_getat_c
            type(c_ptr), value :: vector
            integer(c_int), value :: index
            logical(c_bool), value :: newmem
        end function

!        !bool qvector_setfirst(qvector_t *vector, const void *data);
!        function qvector_setfirst_c(vector, data) bind(c, name="qvector_setfirst")
!            use iso_c_binding, only: c_bool, c_ptr
!            logical(c_bool) :: qvector_setfirst_c
!            type(c_ptr), value :: vector
!            type(c_ptr), value :: data
!        end function
!
!        !bool qvector_setlast(qvector_t *vector, const void *data);
!        function qvector_setlast_c(vector, data) bind(c, name="qvector_setlast")
!            use iso_c_binding, only: c_bool, c_ptr
!            logical(c_bool) :: qvector_setlast_c
!            type(c_ptr), value :: vector
!            type(c_ptr), value :: data
!        end function

        !bool qvector_setat(qvector_t *vector, int index, const void *data);
        function qvector_setat_c(vector, index, data) bind(c, name="qvector_setat")
            use iso_c_binding, only: c_bool, c_ptr, c_int
            logical(c_bool) :: qvector_setat_c
            type(c_ptr), value :: vector
            integer(c_int), value :: index
            type(c_ptr), value :: data
        end function

!        !void *qvector_popfirst(qvector_t *vector);
!        function qvector_popfirst(vector) bind(c)
!            use iso_c_binding, only: c_ptr
!            type(c_ptr) :: qvector_popfirst
!            type(c_ptr), value :: vector
!        end function
!
!        !void *qvector_poplast(qvector_t *vector);
!        function qvector_poplast(vector) bind(c)
!            use iso_c_binding, only: c_ptr
!            type(c_ptr) :: qvector_poplast
!            type(c_ptr), value :: vector
!        end function

        !void *qvector_popat(qvector_t *vector, int index);
        function qvector_popat_c(vector, index) bind(c, name="qvector_popat")
            use iso_c_binding, only: c_ptr, c_int
            type(c_ptr) :: qvector_popat_c
            type(c_ptr), value :: vector
            integer(c_int), value :: index
        end function

!        !bool qvector_removefirst(qvector_t *vector);
!        function qvector_removefirst(vector) bind(c)
!            use iso_c_binding, only: c_bool, c_ptr
!            logical(c_bool) :: qvector_removefirst
!            type(c_ptr), value :: vector
!        end function
!
!        !bool qvector_removelast(qvector_t *vector);
!        function qvector_removelast(vector) bind(c)
!            use iso_c_binding, only: c_bool, c_ptr
!            logical(c_bool) :: qvector_removelast
!            type(c_ptr), value :: vector
!        end function

        !bool qvector_removeat(qvector_t *vector, int index);
        function qvector_removeat_c(vector, index) bind(c, name="qvector_removeat")
            use iso_c_binding, only: c_bool, c_ptr, c_int
            logical(c_bool) :: qvector_removeat_c
            type(c_ptr), value :: vector
            integer(c_int), value :: index
        end function

        !size_t qvector_size(qvector_t *vector);
        function qvector_size_c(vector) bind(c, name="qvector_size")
            use iso_c_binding, only: c_size_t, c_ptr
            integer(c_size_t) :: qvector_size_c
            type(c_ptr), value :: vector
        end function

        !bool qvector_resize(qvector_t *vector, size_t newmax);
        function qvector_resize_c(vector, newmax) bind(c, name="qvector_resize")
            use iso_c_binding, only: c_bool, c_ptr, c_size_t
            logical(c_bool) :: qvector_resize_c
            type(c_ptr), value :: vector
            integer(c_size_t), value :: newmax
        end function

!        !void *qvector_toarray(qvector_t *vector, size_t *size);
!        function qvector_toarray(vector, size) bind(c)
!            use iso_c_binding, only: c_ptr
!            type(c_ptr) :: qvector_toarray
!            type(c_ptr), value :: vector
!            type(c_ptr), value :: size
!        end function

!        !void qvector_lock(qvector_t *vector);
!        subroutine qvector_lock(vector) bind(c)
!            use iso_c_binding, only: c_ptr
!            type(c_ptr), value :: vector
!        end subroutine
!
!        !void qvector_unlock(qvector_t *vector);
!        subroutine qvector_unlock(vector) bind(c)
!            use iso_c_binding, only: c_ptr
!            type(c_ptr), value :: vector
!        end subroutine

        !void qvector_clear(qvector_t *vector);
        subroutine qvector_clear_c(vector) bind(c, name="qvector_clear")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: vector
        end subroutine

!        !bool qvector_debug(qvector_t *vector, FILE *out);
!        function qvector_debug(vector, out) bind(c)
!            use iso_c_binding, only: c_bool, c_ptr
!            logical(c_bool) :: qvector_debug
!            type(c_ptr), value :: vector
!            type(c_ptr), value :: out
!        end function

        !void qvector_free(qvector_t *vector);
        subroutine qvector_free_c(vector) bind(c, name="qvector_free")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: vector
        end subroutine

        !void qvector_reverse(qvector_t *vector);
        subroutine qvector_reverse_c(vector) bind(c, name="qvector_reverse")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: vector
        end subroutine

        !bool qvector_getnext(qvector_t *vector, qvector_obj_t *obj, bool newmem);
        function qvector_getnext_c(vector, obj, newmem) bind(c, name="qvector_getnext")
            use iso_c_binding, only: c_bool, c_ptr
            logical(c_bool) :: qvector_getnext_c
            type(c_ptr), value :: vector
            type(c_ptr), value :: obj
            logical(c_bool), value :: newmem
        end function
    end interface

    interface
        !void qvector_copy_c(qhashtbl_t **to, qhashtbl_t *from)
        subroutine qvector_copy_c(to, from) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr) :: to
            type(c_ptr), value :: from
        end subroutine
    end interface

end module qvector_interfaces_m

module qvector_m
    use iso_c_binding
    use qvector_interfaces_m
    use qlibc_util_m
    use qvector_util_m
    implicit none

    type, public :: qvector_t
        private
        type(c_ptr)       :: q_cp = c_null_ptr
        integer(c_size_t) :: size_data

        logical :: was_init = .false.
    contains
        final :: qvector_free
        procedure :: new => qvector_new                 !< Constructor of qvector
        procedure :: addfirst => qvector_addfirst       !< Insert an element at the beginning of this vector.
        procedure :: addlast => qvector_addlast         !< Insert an element at the end of this vector.
        procedure :: addat => qvector_addat             !< Inserts a element at the specified position in this vector.
        procedure :: getfirst => qvector_getfirst       !< Returns the first element in this vector.
        procedure :: getlast => qvector_getlast         !< Returns the last element in this vector.
        procedure :: getat => qvector_getat             !< Returns the element at the specified position in this vector.
        procedure :: setfirst => qvector_setfirst       !< Set the first element with a new value in this vector.
        procedure :: setlast => qvector_setlast         !< Set the last element with a new value in this vector.
        procedure :: setat => qvector_setat             !< Set new value at the specified position in this vector.
        procedure :: popfirst => qvector_popfirst       !< Returns and remove the first element in this vector.
        procedure :: poplast => qvector_poplast         !< Returns and remove the last element in this vector.
        procedure :: popat => qvector_popat             !< Returns and remove the element at the specified position in this vector.
        procedure :: removefirst => qvector_removefirst !< Removes the first element in this vector.
        procedure :: removelast => qvector_removelast   !< Removes the last element in this vector.
        procedure :: removeat => qvector_removeat       !< Removes the element at the specified position in this vector.
        procedure :: size => qvector_size               !< Get the number of elements in this vector.
        procedure :: resize => qvector_resize           !< Changes the allocated memory space size.
        procedure :: toarray => qvector_toarray         !< Returns an array contains all the elements in this vector.
        procedure :: clear => qvector_clear             !< Remove all the elements in this vector.
        procedure :: reverse => qvector_reverse         !< Reverse the order of element in this vector.
        procedure :: getnext => qvector_getnext         !< Get next object in this vector.
    end type

    integer, public :: QVECTOR_RESIZE_DOUBLE = ishft(1, 1) !< double the size when vector is full
    integer, public :: QVECTOR_RESIZE_LINEAR = ishft(1, 2) !< add the size with initial num when vector is full
    integer, public :: QVECTOR_RESIZE_EXACT  = ishft(1, 3) !< add up as much as needed

    ! Type describes a one object of the qvector
    type, public :: qvector_obj_t
        private
        type(c_ptr) :: qobj = c_null_ptr
        integer(c_size_t) :: objsize

        logical :: wasInit = .false.
        ! It seems, that gfortran has a bug, which prevents qobj to be initialized to c_null_ptr here.
        ! variable wasInit is used as a workaround.
    contains
        final :: qvector_objfree
        procedure :: getdata => qvector_getdata !< Get value from the vector object.
        procedure :: init => qvector_objinit    !< Initialize the vector object.
    end type

    interface assignment(=)
        module procedure qvector_copy
        module procedure qvector_obj_copy
    end interface

    private
    public :: qvector_new, assignment(=)

contains

    !> @brief qtreetbl constructor
    !!
    subroutine qvector_new(self, imax, size_data, resize_opt)
        class(qvector_t) :: self
        integer, intent(in) :: imax !< initial max size of the vector
        integer, intent(in) :: size_data !< size of each element
        integer, intent(in) :: resize_opt !< option for resize, one from QVECTOR_RESIZE_...

        self%size_data = int(size_data, kind=c_size_t)
        self%q_cp = qvector_c(int(imax, kind=c_size_t), self%size_data, int(resize_opt, kind=c_int))

        self%was_init = .true.
    end subroutine

    !> @brief qtreetbl destructor
    !!
    subroutine qvector_free(self)
        type(qvector_t), intent(inout) :: self

        !if (c_associated(self%q_cp)) then
        if (self%was_init) then
            call qvector_free_c(self%q_cp)
        end if
    end subroutine

    !> @brief Insert an element at the beginning of this vector.
    !!
    subroutine qvector_addfirst(self, val_data, success)
        class(qvector_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(in) :: val_data   !< value which is inserted
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c

        success_c = qvector_addfirst_c(self%q_cp, c_loc(val_data))
        if (present(success)) success = success_c
    end subroutine

    !> @brief Insert an element at the end of this vector.
    !!
    subroutine qvector_addlast(self, val_data, success)
        class(qvector_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(in) :: val_data  !< value which is inserted
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c

        success_c = qvector_addlast_c(self%q_cp, c_loc(val_data))
        if (present(success)) success = success_c
    end subroutine

    !> @brief Inserts a element at the specified position in this vector.
    !!
    subroutine qvector_addat(self, idx, val_data, success)
        class(qvector_t), intent(inout) :: self
        integer, intent(in)            :: idx      !< index (1...size+1) at which the specified element is to be inserted
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(in) :: val_data  !< value which is inserted
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c
        integer(c_int)  :: idx_c

        if (idx > 0) then
            idx_c = idx - 1
        else if (idx < 0) then
            idx_c = idx
        else ! (idx == 0)
            if (present(success)) success = .false.
            return
        end if
        success_c = qvector_addat_c(self%q_cp, idx_c, c_loc(val_data))
        if (present(success)) success = success_c
    end subroutine

    !> @brief Returns the first element in this vector.
    !!
    subroutine qvector_getfirst(self, val_data, success)
        class(qvector_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(inout) :: val_data  !< value of the first element
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: newmem = .false.
        type(c_ptr)     :: val_data_p

        val_data_p = qvector_getat_c(self%q_cp, 0_c_int, newmem)
        if (c_associated(val_data_p)) then
            call qlibc_copy_data_c(val_data_p, c_loc(val_data), self%size_data, newmem)
            if (present(success)) success = .true.
        else
            if (present(success)) success = .false.
        end if
    end subroutine

    !> @brief Returns the last element in this vector.
    !!
    subroutine qvector_getlast(self, val_data, success)
        class(qvector_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(inout) :: val_data  !< value of the last element
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: newmem = .false.
        type(c_ptr)     :: val_data_p

        val_data_p = qvector_getat_c(self%q_cp, -1_c_int, newmem)
        if (c_associated(val_data_p)) then
            call qlibc_copy_data_c(val_data_p, c_loc(val_data), self%size_data, newmem)
            if (present(success)) success = .true.
        else
            if (present(success)) success = .false.
        end if
    end subroutine

    !> @brief Returns the element at the specified position in this vector.
    !!
    subroutine qvector_getat(self, idx, val_data, success)
        class(qvector_t), intent(inout) :: self
        integer, intent(in)            :: idx      !< index (1...max) at which the value should be returned
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(inout) :: val_data  !< return value at index idx
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success              !< returns .true. in case of success, .false. if error

        logical(c_bool) :: newmem = .false.
        integer(c_int)  :: idx_c
        type(c_ptr)     :: val_data_p

        if (idx > 0) then
            idx_c = idx - 1
        else if (idx < 0) then
            idx_c = idx
        else ! (idx == 0)
            if (present(success)) success = .false.
            return
        end if
        val_data_p = qvector_getat_c(self%q_cp, idx_c, newmem)
        if (c_associated(val_data_p)) then
            call qlibc_copy_data_c(val_data_p, c_loc(val_data), self%size_data, newmem)
            if (present(success)) success = .true.
        else
            if (present(success)) success = .false.
        end if
    end subroutine

    !> @brief Set the first element with a new value in this vector.
    !!
    subroutine qvector_setfirst(self, val_data, success)
        class(qvector_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(in) :: val_data   !< value which is set to the first element
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success           !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c

        success_c = qvector_setat_c(self%q_cp, 0_c_int, c_loc(val_data))
        if (present(success)) success = success_c
    end subroutine

    !> @brief Set the last element with a new value in this vector.
    !!
    subroutine qvector_setlast(self, val_data, success)
        class(qvector_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(in) :: val_data   !< value which is set to the last element
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c

        success_c = qvector_setat_c(self%q_cp, -1_c_int, c_loc(val_data))
        if (present(success)) success = success_c
    end subroutine

    !> @brief Set new value at the specified position in this vector.
    !!
    subroutine qvector_setat(self, idx, val_data, success)
        class(qvector_t), intent(inout) :: self
        integer, intent(in)            :: idx      !< index (1...size+1) at which the specified element is to set
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(in) :: val_data   !< value which is set
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c
        integer(c_int)  :: idx_c

        if (idx > 0) then
            idx_c = idx - 1
        else if (idx < 0) then
            idx_c = idx
        else ! (idx == 0)
            if (present(success)) success = .false.
            return
        end if
        success_c = qvector_setat_c(self%q_cp, idx_c, c_loc(val_data))
        if (present(success)) success = success_c
    end subroutine

    !> @brief Returns and remove the last element in this vector.
    !!
    subroutine qvector_poplast(self, val_data, success)
        class(qvector_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(inout) :: val_data   !< return last value of the vector, which was removed
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: freemem = .true.
        type(c_ptr)     :: val_data_p

        val_data_p = qvector_popat_c(self%q_cp, -1_c_int)
        if (c_associated(val_data_p)) then
            call qlibc_copy_data_c(val_data_p, c_loc(val_data), self%size_data, freemem)
            if (present(success)) success = .true.
        else
            if (present(success)) success = .false.
        end if
    end subroutine

    !> @brief Returns and remove the first element in this vector.
    !!
    subroutine qvector_popfirst(self, val_data, success)
        class(qvector_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(inout) :: val_data   !< return first value of the vector, which was removed
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: freemem = .true.
        type(c_ptr)     :: val_data_p

        val_data_p = qvector_popat_c(self%q_cp, 0_c_int)
        if (c_associated(val_data_p)) then
            call qlibc_copy_data_c(val_data_p, c_loc(val_data), self%size_data, freemem)
            if (present(success)) success = .true.
        else
            if (present(success)) success = .false.
        end if
    end subroutine

    !> @brief Returns and remove the element at the specified position in this vector.
    !!
    subroutine qvector_popat(self, idx, val_data, success)
        class(qvector_t), intent(inout) :: self
        integer, intent(in)            :: idx      !< index (1...size) at which the element is to be removed
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(inout) :: val_data   !< return value which was removed
#else
#include "nocheck_val_data.fi"
#endif
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: freemem = .true.
        integer(c_int)  :: idx_c
        type(c_ptr)     :: val_data_p

        if (idx > 0) then
            idx_c = idx - 1
        else if (idx < 0) then
            idx_c = idx
        else ! (idx == 0)
            if (present(success)) success = .false.
            return
        end if
        val_data_p = qvector_popat_c(self%q_cp, idx_c)
        if (c_associated(val_data_p)) then
            call qlibc_copy_data_c(val_data_p, c_loc(val_data), self%size_data, freemem)
            if (present(success)) success = .true.
        else
            if (present(success)) success = .false.
        end if
    end subroutine

    !> @brief Removes the first element in this vector.
    !!
    subroutine qvector_removefirst(self, success)
        class(qvector_t), intent(inout) :: self
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c

        success_c = qvector_removeat_c(self%q_cp, 0_c_int)
        if (present(success)) success = success_c
    end subroutine

    !> @brief Removes the last element in this vector.
    !!
    subroutine qvector_removelast(self, success)
        class(qvector_t), intent(inout) :: self
        logical, optional, intent(out) :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c

        success_c = qvector_removeat_c(self%q_cp, -1_c_int)
        if (present(success)) success = success_c
    end subroutine

    !> @brief Removes the element at the specified position in this vector.
    !!
    subroutine qvector_removeat(self, idx, success)
        class(qvector_t), intent(inout) :: self
        integer, intent(in)            :: idx      !< index (1...size) at which the specified element is to be removed
        logical, optional, intent(out) :: success  !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c
        integer(c_int)  :: idx_c

        if (idx > 0) then
            idx_c = idx - 1
        else if (idx < 0) then
            idx_c = idx
        else ! (idx == 0)
            if (present(success)) success = .false.
            return
        end if
        success_c = qvector_removeat_c(self%q_cp, idx_c)
        if (present(success)) success = success_c
    end subroutine

    !> @brief Get the number of elements in this vector.
    !!
    function qvector_size(self)
        class(qvector_t), intent(inout) :: self
        integer :: qvector_size
        integer, parameter :: ik = kind(1)

        qvector_size = int(qvector_size_c(self%q_cp), kind=ik)
    end function

    !> @brief Changes the allocated memory space size.
    !!
    subroutine qvector_resize(self, newmax, success)
        class(qvector_t), intent(inout) :: self
        integer, intent(in)             :: newmax    !< the new max number of elements.
        logical, optional, intent(out)  :: success   !< returns .true. in case of success, .false. if error

        logical(c_bool) :: success_c

        success_c = qvector_resize_c(self%q_cp, int(newmax, kind=c_size_t))
        if (present(success)) success = success_c
    end subroutine

    !> @brief Returns an array contains all the elements in this vector.
    !!
    subroutine qvector_toarray(self, val_data)
        class(qvector_t), intent(inout) :: self
#ifndef USE_COMPILER_DIRECTIVE
        type(*), dimension(*), target, intent(inout) :: val_data   !< return array which is filled with values
#else
#include "nocheck_val_data.fi"
#endif

        call qvector_toarray_c(self%q_cp, c_loc(val_data))
    end subroutine

    !> @brief Remove all the elements in this vector.
    !!
    subroutine qvector_clear(self)
        class(qvector_t), intent(inout) :: self

        call qvector_clear_c(self%q_cp)
    end subroutine

    !> @brief Reverse the order of element in this vector.
    !!
    subroutine qvector_reverse(self)
        class(qvector_t), intent(inout) :: self

        call qvector_reverse_c(self%q_cp)
    end subroutine

    !> @brief Get next object in this vector.
    !!
    logical function qvector_getnext(self, obj)
        class(qvector_t), intent(inout) :: self
        type(qvector_obj_t), intent(inout) :: obj

        logical(c_bool) :: newmem = .false.

        if (.not. obj%wasInit) then
        !if (.not. c_associated(obj%qobj)) then
            call qvector_objinit(obj)
        end if

        qvector_getnext = qvector_getnext_c(self%q_cp, obj%qobj, newmem)
        obj%objsize = self%size_data
    end function

    !> @brief Copy procedure.
    !!
    subroutine qvector_copy(to, from)
        type(qvector_t), intent(inout) :: to
        type(qvector_t), intent(in)    :: from

        if (.not. to%was_init) then
            to%q_cp = c_null_ptr
            to%was_init = .true.
        end if
        call qvector_copy_c(to%q_cp, from%q_cp)
        to%size_data = from%size_data
    end subroutine



    !**********************************************
    !*** Procedures for work with qvector_obj_t
    !**********************************************

    !> @brief Initialize the vector object.
    !!
    subroutine qvector_objinit(obj)
        class(qvector_obj_t), intent(inout) :: obj

        if (.not. obj%wasInit) then
            obj%wasInit = .true.
            obj%qobj = c_null_ptr
        end if

        if ( .not. c_associated(obj%qobj)) then
            obj%qobj = qvector_getobj_c()
        else
            call qvector_objinit_c(obj%qobj)
        end if
    end subroutine

    !> @brief Destructor of the vector object.
    !!
    subroutine qvector_objfree(obj)
        type(qvector_obj_t), intent(inout) :: obj

        if (.not. obj%wasInit) then
            obj%qobj = c_null_ptr
            obj%wasInit = .true.
        end if
        if (c_associated(obj%qobj)) then
            call qlibc_free_c(obj%qobj)
            obj%qobj = c_null_ptr
        end if
    end subroutine

    !> @brief Copy procedure for vector object. Should NOT be used.
    !!
    subroutine qvector_obj_copy(to, from)
        type(qvector_obj_t), intent(inout) :: to
        type(qvector_obj_t), intent(in)    :: from

        print *, "Error. qvector_obj_t object can not be copied."
        print *, "Sub: qvector_obj_copy"
        stop "Program terminated!"
    end subroutine

    !> @brief Get value from the vector object.
    !!
    subroutine qvector_getdata(obj, val_data)
        class(qvector_obj_t) :: obj
#ifndef USE_COMPILER_DIRECTIVE
        type(*), target, intent(inout) :: val_data   !< return array which is filled with values
#else
#include "nocheck_val_data.fi"
#endif

        call qvector_getdata_c(obj%qobj, c_loc(val_data), obj%objsize)
    end subroutine

end module qvector_m

#ifdef USE_COMPILER_DIRECTIVE
#undef USE_COMPILER_DIRECTIVE
#endif
