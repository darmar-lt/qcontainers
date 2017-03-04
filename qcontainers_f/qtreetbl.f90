!===========================================
!
!===========================================
module qtreetbl_util_m
    use iso_c_binding
    use qlibc_util_m
    implicit none

    interface
        type(c_ptr) function qtreetbl_getobj_c() bind(c)
            use iso_c_binding, only: c_ptr
        end function

        subroutine qtreetbl_objinit_c(obj) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
        end subroutine

        subroutine qtreetbl_getdata_c(obj, val_data) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
            include "nocheck_val_data.fi"
            real, dimension(*) :: val_data
        end subroutine

        subroutine qtreetbl_getname_c(obj, name) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
            include "nocheck_name.fi"
            real, dimension(*) :: name
        end subroutine

        integer(c_size_t) function qtreetbl_getnamesize_c(obj) bind(c)
            use iso_c_binding, only: c_ptr, c_size_t
            type(c_ptr), value :: obj
        end function

        subroutine qtreetbl_find_nearest_c(tbl, name, namesize, newmem, obj, found) bind(c)
            use iso_c_binding, only: c_ptr, c_size_t, c_bool
            type(c_ptr), value :: tbl
            include "nocheck_name.fi"
            real, dimension(*) :: name
            integer(c_size_t), value :: namesize
            logical(c_bool), value :: newmem
            type(c_ptr), value :: obj
            logical(c_bool) :: found
        end subroutine
    end interface

    interface
        !void qtreetbl_copy_c(qtreetbl_t **to, qtreetbl_t *from)
        subroutine qtreetbl_copy_c(to, from) bind(c)
            use iso_c_binding, only: c_ptr
            type(c_ptr) :: to
            type(c_ptr), value :: from
        end subroutine
    end interface
end module qtreetbl_util_m

!===========================================
!
!===========================================
module qtreetbl_interfaces_m
    implicit none

    interface

        !qtreetbl_t *qtreetbl(int options);
        function qtreetbl_c(options) bind(c, name="qtreetbl")
            use iso_c_binding, only: c_ptr, c_int
            type(c_ptr) :: qtreetbl_c
            integer(c_int), value :: options
        end function

        !void qtreetbl_set_compare( qtreetbl_t *tbl, int (*cmp)(const void *name1, size_t namesize1, const void *name2, size_t namesize2));
        subroutine qtreetbl_set_compare() bind(c)
            use iso_c_binding, only:
        end subroutine

        !bool qtreetbl_put(qtreetbl_t *tbl, const char *name, const void *data, size_t datasize);
        function qtreetbl_put_c(tbl, name, val_data, datasize) bind(c, name="qtreetbl_put")
            use iso_c_binding, only: c_bool, c_ptr, c_char, c_size_t
            logical(c_bool) :: qtreetbl_put_c
            type(c_ptr), value :: tbl
            character(kind=c_char), dimension(*) :: name
            include "nocheck_val_data.fi"
            real, dimension(*)       :: val_data
            integer(c_size_t), value :: datasize
        end function

!        !bool qtreetbl_putstr(qtreetbl_t *tbl, const char *name, const char *str);
!        function qtreetbl_putstr(tbl, name, str) bind(c)
!            use iso_c_binding, only: c_bool, c_ptr, c_char
!            logical(c_bool) :: qtreetbl_putstr
!            type(c_ptr), value :: tbl
!            character(kind=c_char), dimension(*) :: name
!            character(kind=c_char), dimension(*) :: str
!        end function

        !bool qtreetbl_put_by_obj(qtreetbl_t *tbl, const void *name, size_t namesize, const void *data, size_t datasize);
        function qtreetbl_put_by_obj_c(tbl, name, namesize, data, datasize) bind(c,name="qtreetbl_put_by_obj")
            use iso_c_binding, only: c_bool, c_ptr, c_size_t
            logical(c_bool) :: qtreetbl_put_by_obj_c
            type(c_ptr), value :: tbl
            type(c_ptr), value :: name
            integer(c_size_t), value :: namesize
            type(c_ptr), value :: data
            integer(c_size_t), value :: datasize
        end function

        !void *qtreetbl_get(qtreetbl_t *tbl, const char *name, size_t *datasize, bool newmem);
        function qtreetbl_get_c(tbl, name, datasize, newmem) bind(c, name="qtreetbl_get")
            use iso_c_binding, only: c_ptr, c_char, c_bool
            type(c_ptr) :: qtreetbl_get_c
            type(c_ptr), value :: tbl
            character(kind=c_char), dimension(*) :: name
            type(c_ptr), value :: datasize
            logical(c_bool), value :: newmem
        end function

!        !char *qtreetbl_getstr(qtreetbl_t *tbl, const char *name, const bool newmem);
!        function qtreetbl_getstr(tbl, name, newmem) bind(c)
!            use iso_c_binding, only: c_ptr, c_char, c_bool
!            type(c_ptr) :: qtreetbl_getstr
!            type(c_ptr), value :: tbl
!            character(kind=c_char), dimension(*) :: name
!            logical(c_bool), value :: newmem
!        end function

        !void *qtreetbl_get_by_obj(qtreetbl_t *tbl, const char *name, size_t namesize, size_t *datasize, bool newmem);
        function qtreetbl_get_by_obj_c(tbl, name, namesize, datasize, newmem) bind(c, name="qtreetbl_get_by_obj")
            use iso_c_binding, only: c_ptr, c_char, c_size_t, c_bool
            type(c_ptr) :: qtreetbl_get_by_obj_c
            type(c_ptr), value :: tbl
            type(c_ptr), value :: name
            !character(kind=c_char), dimension(*) :: name
            integer(c_size_t), value :: namesize
            type(c_ptr), value :: datasize
            logical(c_bool), value :: newmem
        end function

!        !bool qtreetbl_remove(qtreetbl_t *tbl, const char *name);
!        function qtreetbl_remove(tbl, name) bind(c)
!            use iso_c_binding, only: c_bool, c_ptr, c_char
!            logical(c_bool) :: qtreetbl_remove
!            type(c_ptr), value :: tbl
!            character(kind=c_char), dimension(*) :: name
!        end function

        !bool qtreetbl_remove_by_obj(qtreetbl_t *tbl, const void *name, size_t namesize);
        function qtreetbl_remove_by_obj_c(tbl, name, namesize) bind(c, name="qtreetbl_remove_by_obj")
            use iso_c_binding, only: c_bool, c_ptr, c_size_t
            logical(c_bool) :: qtreetbl_remove_by_obj_c
            type(c_ptr), value :: tbl
            type(c_ptr), value :: name
            integer(c_size_t), value :: namesize
        end function

        !bool qtreetbl_getnext(qtreetbl_t *tbl, qtreetbl_obj_t *obj, const bool newmem);
        function qtreetbl_getnext_c(tbl, obj, newmem) bind(c, name="qtreetbl_getnext")
            use iso_c_binding, only: c_bool, c_ptr
            logical(c_bool) :: qtreetbl_getnext_c
            type(c_ptr), value :: tbl
            type(c_ptr), value :: obj
            logical(c_bool), value :: newmem
        end function

        !void *qtreetbl_find_min(qtreetbl_t *tbl, size_t *namesize);
        function qtreetbl_find_min_c(tbl, namesize) bind(c, name="qtreetbl_find_min")
            use iso_c_binding, only: c_ptr
            type(c_ptr) :: qtreetbl_find_min_c
            type(c_ptr), value :: tbl
            type(c_ptr), value :: namesize
        end function

        !void *qtreetbl_find_max(qtreetbl_t *tbl, size_t *namesize);
        function qtreetbl_find_max_c(tbl, namesize) bind(c, name="qtreetbl_find_max")
            use iso_c_binding, only: c_ptr
            type(c_ptr) :: qtreetbl_find_max_c
            type(c_ptr), value :: tbl
            type(c_ptr), value :: namesize
        end function


        !size_t qtreetbl_size(qtreetbl_t *tbl);
        function qtreetbl_size_c(tbl) bind(c, name="qtreetbl_size")
            use iso_c_binding, only: c_size_t, c_ptr
            integer(c_size_t) :: qtreetbl_size_c
            type(c_ptr), value :: tbl
        end function

        !void qtreetbl_clear(qtreetbl_t *tbl);
        subroutine qtreetbl_clear_c(tbl) bind(c, name="qtreetbl_clear")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: tbl
        end subroutine

!        !void qtreetbl_lock(qtreetbl_t *tbl);
!        subroutine qtreetbl_lock_c(tbl) bind(c, name="qtreetbl_lock")
!            use iso_c_binding, only: c_ptr
!            type(c_ptr), value :: tbl
!        end subroutine
!
!        !void qtreetbl_unlock(qtreetbl_t *tbl);
!        subroutine qtreetbl_unlock_c(tbl) bind(c, name="qtreetbl_unlock")
!            use iso_c_binding, only: c_ptr
!            type(c_ptr), value :: tbl
!        end subroutine

        !void qtreetbl_free(qtreetbl_t *tbl);
        subroutine qtreetbl_free_c(tbl) bind(c, name="qtreetbl_free")
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: tbl
        end subroutine

    end interface
end module qtreetbl_interfaces_m


module qtreetbl_m
    use iso_c_binding
    use qtreetbl_interfaces_m
    use qlibc_util_m
    use qtreetbl_util_m
    implicit none


    type, public :: qtreetbl_t
        private
        type(c_ptr) :: q_cp = c_null_ptr
        integer(c_size_t) :: size_data

        logical :: was_init = .false.
    contains
        final :: qtreetbl_free
        procedure :: new => qtreetbl_new                             ! Constructor of the container. Should be called before use of the container.
        procedure :: clear => qtreetbl_clear                         ! Remove all values from the tree.
        procedure :: find_max => qtreetbl_find_max                   ! Find the name of very right object.
        procedure :: find_max_by_obj => qtreetbl_find_max_by_obj     ! Find the name of very right object.
        procedure :: find_min => qtreetbl_find_min                   ! Find the name of very left object.
        procedure :: find_min_by_obj => qtreetbl_find_min_by_obj     ! Find the name of very left object.
        procedure :: find_nearest => qtreetbl_find_nearest           ! Find nearest object to the given name.
        procedure :: get => qtreetbl_get                             ! Get data.
        procedure :: get_by_obj => qtreetbl_get_by_obj               ! Get data.
        procedure :: getnext => qtreetbl_getnext                     ! Get next object.
        procedure :: getstr => qtreetbl_getstr                       ! Get string data.
        procedure :: haskey => qtreetbl_haskey                       ! Return if the key is in the container.
        procedure :: haskey_by_obj => qtreetbl_haskey_by_obj         ! Return if the key is in the container.
        procedure :: put => qtreetbl_put                             ! Put key-value pair to the container.
        procedure :: put_by_obj => qtreetbl_put_by_obj               ! Put key-value pair to the container.
        procedure :: putstr => qtreetbl_putstr                       ! Put string to the container.
        procedure :: remove => qtreetbl_remove                       ! Remove key from the container.
        procedure :: remove_by_obj => qtreetbl_remove_by_obj         ! Remove key from the container.
        procedure :: size => qtreetbl_size                           ! Get current size (number of pairs) in the container.
    end type

    ! Type describes a one object of the treetbl
    type, public :: qtreetbl_obj_t
        type(c_ptr) :: qobj = c_null_ptr
        logical :: wasInit = .false.
        ! It seems, that gfortran has a bug, which prevents qobj to be initialized to c_null_ptr here.
        ! variable wasInit is used as a workaround.
    contains
        final :: qtreetbl_objfree
        procedure :: getdata => qtreetbl_getdata                !< Get data from the object.
        procedure :: getname => qtreetbl_getname                !< Get name from the object.
        procedure :: getname_by_obj => qtreetbl_getname_by_obj  !< Get name from the object.
        procedure :: init => qtreetbl_objinit                   !< Initialize the object.
    end type

    interface assignment(=)
        module procedure qtreetbl_copy
        module procedure qtreetbl_obj_copy
    end interface

    private
    public :: assignment(=)

contains

    !> @brief qtreetbl constructor.
    !!
    subroutine qtreetbl_new(self, size_data)
        class(qtreetbl_t) :: self
        integer, optional, intent(in) :: size_data !< size of each element in bytes

        integer(c_int) :: opt

        opt = 0
        self%q_cp = qtreetbl_c(opt)
        if (present(size_data)) then
            self%size_data = int(size_data, kind=c_size_t)
        else
            self%size_data = 0
        end if

        self%was_init = .true.
    end subroutine

    !> @brief qtreetbl destructor.
    !!
    subroutine qtreetbl_free(self)
        type(qtreetbl_t), intent(inout) :: self

        if (.not. self%was_init) then
            self%q_cp = c_null_ptr
            self%was_init = .true.
        end if

        if (c_associated(self%q_cp)) then
            call qtreetbl_free_c(self%q_cp)
            self%q_cp = c_null_ptr
        end if
    end subroutine

    !> @brief Put key-value pair to the container.
    !!
    subroutine qtreetbl_put(self, name, val_data, size_data, success)
        class(qtreetbl_t), intent(inout) :: self
        character(len=*), target, intent(in) :: name !< key name.
        include "nocheck_val_data.fi"
        real, dimension(*), target, intent(in) :: val_data    !< data to put
        integer, optional, intent(in)  :: size_data   !< size of data object
        logical, optional, intent(out) :: success    !< true if successful

        integer(c_size_t) :: sd
        logical(c_bool) :: suc_c

        if (present(size_data)) then
            sd = int(size_data, kind=c_size_t)
        else
            sd = self%size_data
        end if
        suc_c = qtreetbl_put_by_obj_c(self%q_cp, c_loc(name), len_trim(name, kind=c_size_t), c_loc(val_data), sd)
        if (present(success)) success = suc_c
    end subroutine


    !> @brief Put string to the tree.
    !!
    subroutine qtreetbl_putstr(self, name, str_data, success)
        class(qtreetbl_t), intent(inout) :: self
        character(len=*), target, intent(in)     :: name     !< key name.
        character(len=*), target, intent(in)     :: str_data !< string to put.
        logical, optional, intent(out)           :: success  !< true if successful.

        integer(c_size_t) :: sd
        logical(c_bool) :: suc_c

        sd = len(str_data, kind=c_size_t)

        suc_c = qtreetbl_put_by_obj_c(self%q_cp, c_loc(name), len_trim(name, kind=c_size_t), c_loc(str_data), sd)
        if (present(success)) success = suc_c
    end subroutine

    !> @brief Put key-value pair to the container.
    !!
    subroutine qtreetbl_put_by_obj(self, name, name_size, val_data, size_data, success)
        class(qtreetbl_t), intent(inout) :: self
        include "nocheck_name.fi"
        real, dimension(*), target, intent(in) :: name  !< key name.
        integer, intent(in) :: name_size                !< size of the name in bytes.
        include "nocheck_val_data.fi"
        real, dimension(*), target, intent(in) :: val_data  !< data to put.
        integer, optional, intent(in)  :: size_data !< size of data object.
        logical, optional, intent(out) :: success   !< true if successful.

        integer(c_size_t) :: sd
        logical(c_bool) :: suc_c

        if (present(size_data)) then
            sd = int(size_data, kind=c_size_t)
        else
            sd = self%size_data
        end if
        suc_c = qtreetbl_put_by_obj_c(self%q_cp, c_loc(name), int(name_size,kind=c_size_t), c_loc(val_data), &
                                        int(size_data,kind=c_size_t))
        if (present(success)) success = suc_c
    end subroutine

    !> @brief Get data.
    !!
    subroutine qtreetbl_get(self, name, val_data, found)
        class(qtreetbl_t), intent(in) :: self
        character(len=*), target, intent(in) :: name  !< key name.
        include "nocheck_val_data.fi"
        real, dimension(*)   :: val_data              !< returned data.
        logical, intent(out) :: found                 !< true if key was found.

        integer(c_size_t), target :: size_data
        type(c_ptr) :: size_data_p
        type(c_ptr) :: val_data_p
        logical(c_bool) :: newmem = .false.

        size_data_p = c_loc(size_data)
        val_data_p = qtreetbl_get_by_obj_c(self%q_cp, c_loc(name), len_trim(name, kind=c_size_t), size_data_p, newmem)
        if (c_associated(val_data_p)) then
            call qlibc_copy_data_c(val_data_p, val_data, size_data, newmem)
            found = .true.
        else
            found = .false.
        end if
    end subroutine

    !> @brief Get string data.
    !!
    subroutine qtreetbl_getstr(self, name, str_data, found)
        class(qtreetbl_t), intent(in) :: self
        character(len=*), target, intent(in)  :: name             !< key name.
        character(len=:), allocatable, intent(inout) :: str_data  !< returned string.
        logical, intent(out) :: found                             !< true if key was found.

        integer(c_size_t), target :: size_data
        type(c_ptr) :: size_data_p
        type(c_ptr) :: val_data_p
        logical(c_bool) :: newmem = .false.

        size_data_p = c_loc(size_data)
        val_data_p = qtreetbl_get_by_obj_c(self%q_cp, c_loc(name), len_trim(name, kind=c_size_t), size_data_p, newmem)
        if (c_associated(val_data_p)) then
            if (allocated(str_data)) then
                if (len(str_data) /= size_data) then
                    deallocate(str_data)
                    allocate(character(len=size_data) :: str_data)
                end if
            else
                allocate(character(len=size_data) :: str_data)
            end if
            call qlibc_copy_data_c(val_data_p, str_data, size_data, newmem)
            found = .true.
        else
            found = .false.
        end if
    end subroutine

    !> @brief Get data.
    !!
    subroutine qtreetbl_get_by_obj(self, name, name_size, val_data, found)
        class(qtreetbl_t), intent(in) :: self
        include "nocheck_name.fi"
        real, dimension(*), target, intent(in) :: name   !< key name.
        integer, intent(in) :: name_size      !< size of the name in bytes.
        include "nocheck_val_data.fi"
        real, dimension(*)   :: val_data      !< returned data.
        logical, intent(out) :: found         !< true if key was found.

        integer(c_size_t), target :: size_data
        type(c_ptr) :: size_data_p
        type(c_ptr) :: val_data_p
        logical(c_bool) :: newmem = .false.

        size_data_p = c_loc(size_data)
        val_data_p = qtreetbl_get_by_obj_c(self%q_cp, c_loc(name), int(name_size, kind=c_size_t), size_data_p, newmem)
        if (c_associated(val_data_p)) then
            call qlibc_copy_data_c(val_data_p, val_data, size_data, newmem)
            found = .true.
        else
            found = .false.
        end if
    end subroutine

    !> @brief Return if the key is in the container.
    !!
    function qtreetbl_haskey(self, name)
        class(qtreetbl_t), intent(in) :: self
        character(len=*), target, intent(in) :: name !< key name.
        logical :: qtreetbl_haskey                   !< true if key was found.

        type(c_ptr) :: size_data_p
        type(c_ptr) :: val_data_p
        logical(c_bool) :: newmem = .false.

        size_data_p = c_null_ptr
        val_data_p = qtreetbl_get_by_obj_c(self%q_cp, c_loc(name), len_trim(name, kind=c_size_t), size_data_p, newmem)
        if (c_associated(val_data_p)) then
            qtreetbl_haskey = .true.
        else
            qtreetbl_haskey = .false.
        end if
    end function

    !> @brief Return if the key is in the container.
    !!
    function qtreetbl_haskey_by_obj(self, name, name_size)
        class(qtreetbl_t), intent(in) :: self
        include "nocheck_name.fi"
        real, dimension(*), target, intent(in) :: name  !< key name.
        integer, intent(in) :: name_size     !< size of the name in bytes.
        logical :: qtreetbl_haskey_by_obj

        type(c_ptr) :: size_data_p
        type(c_ptr) :: val_data_p
        logical(c_bool) :: newmem = .false.

        size_data_p = c_null_ptr
        val_data_p = qtreetbl_get_by_obj_c(self%q_cp, c_loc(name), int(name_size, kind=c_size_t), size_data_p, newmem)
        if (c_associated(val_data_p)) then
            qtreetbl_haskey_by_obj = .true.
        else
            qtreetbl_haskey_by_obj = .false.
        end if
    end function

    !> @brief Remove key from the container.
    !!
    subroutine qtreetbl_remove(self, name, success)
        class(qtreetbl_t), intent(in) :: self
        character(len=*), target, intent(in) :: name !< key name.
        logical, optional, intent(out) :: success    !< true if successful.

        logical(c_bool) :: suc

        suc = qtreetbl_remove_by_obj_c(self%q_cp, c_loc(name), len_trim(name, kind=c_size_t))
        if (present(success)) success = suc
    end subroutine

    !> @brief Remove key from the container.
    !!
    subroutine qtreetbl_remove_by_obj(self, name, name_size, success)
        class(qtreetbl_t), intent(in) :: self
        include "nocheck_name.fi"
        real, dimension(*), target, intent(in) :: name !< key name.
        integer, intent(in) :: name_size           !< size of the name in bytes.
        logical, optional, intent(out) :: success  !< true if successful.

        logical(c_bool) :: suc

        suc = qtreetbl_remove_by_obj_c(self%q_cp, c_loc(name), int(name_size, kind=c_size_t))
        if (present(success)) success = suc
    end subroutine

    !> @brief Get next object.
    !!
    logical function qtreetbl_getnext(self, obj)
        class(qtreetbl_t), intent(in) :: self
        type(qtreetbl_obj_t), intent(inout) :: obj !< qtreetbl object
        logical(c_bool) :: newmem = .false.

        if (.not. obj%wasInit) then
        !if (.not. c_associated(obj%qobj)) then
            call qtreetbl_objinit(obj)
        end if

        qtreetbl_getnext = qtreetbl_getnext_c(self%q_cp, obj%qobj, newmem)
    end function

    !> @brief Find the name of very left object.
    !!
    subroutine qtreetbl_find_min(self, name, found)
        class(qtreetbl_t), intent(in) :: self
        character(len=:), allocatable, intent(inout) :: name !< found key name.
        logical, intent(out) :: found                        !< true if name was found.

        integer(c_size_t), target :: namesize
        type(c_ptr) :: namec_p
        logical(c_bool) :: freemem = .true.

        namec_p = qtreetbl_find_min_c(self%q_cp, c_loc(namesize))
        if (c_associated(namec_p)) then
            if (allocated(name)) then
                if (len(name) /= namesize) then
                    deallocate(name)
                    allocate(character(len=namesize) :: name)
                end if
            else
                allocate(character(len=namesize) :: name)
            end if
            call qlibc_copy_data_c(namec_p, name, namesize, freemem)
            found = .true.
        else
            found = .false.
        end if
    end subroutine

    !> @brief Find the name of very left object.
    !!
    subroutine qtreetbl_find_min_by_obj(self, name, found)
        class(qtreetbl_t), intent(in) :: self
        include "nocheck_name.fi"
        real, dimension(*), intent(inout)  :: name   !< found key name
        logical, intent(out) :: found    !< true if found

        integer(c_size_t), target :: namesize
        type(c_ptr) :: namec_p
        logical(c_bool) :: freemem = .true.

        namec_p = qtreetbl_find_min_c(self%q_cp, c_loc(namesize))
        if (c_associated(namec_p)) then
            call qlibc_copy_data_c(namec_p, name, namesize, freemem)
            found = .true.
        else
            found = .false.
        end if
    end subroutine

    !> @brief Find the name of very right object.
    !!
    subroutine qtreetbl_find_max(self, name, found)
        class(qtreetbl_t), intent(in) :: self
        character(len=:), allocatable, intent(inout) :: name !< found key name
        logical, intent(out) :: found                        !< true if found

        integer(c_size_t), target :: namesize
        type(c_ptr) :: namec_p
        logical(c_bool) :: freemem = .true.

        namec_p = qtreetbl_find_max_c(self%q_cp, c_loc(namesize))
        if (c_associated(namec_p)) then
            if (allocated(name)) then
                if (len(name) /= namesize) then
                    deallocate(name)
                    allocate(character(len=namesize) :: name)
                end if
            else
                allocate(character(len=namesize) :: name)
            end if
            call qlibc_copy_data_c(namec_p, name, namesize, freemem)
            found = .true.
        else
            found = .false.
        end if
    end subroutine

    !> @brief Find the name of very right object.
    !!
    subroutine qtreetbl_find_max_by_obj(self, name, found)
        class(qtreetbl_t), intent(in) :: self
        include "nocheck_name.fi"
        real, dimension(*), intent(inout)  :: name  !< found key name
        logical, intent(out) :: found   !< true if found

        integer(c_size_t), target :: namesize
        type(c_ptr) :: namec_p
        logical(c_bool) :: freemem = .true.

        namec_p = qtreetbl_find_max_c(self%q_cp, c_loc(namesize))
        if (c_associated(namec_p)) then
            call qlibc_copy_data_c(namec_p, name, namesize, freemem)
            found = .true.
        else
            found = .false.
        end if
    end subroutine

    !> @brief Find equal or nearest object to the given name.
    !!
    subroutine qtreetbl_find_nearest(self, name, namesize, obj, found)
        class(qtreetbl_t), intent(in) :: self
        include "nocheck_name.fi"
        real, dimension(*), intent(inout) :: name   !< key name
        integer(c_size_t), target :: namesize       !< size of the name
        type(qtreetbl_obj_t), intent(inout) :: obj  !< found object
        logical, optional, intent(out) :: found     !> true if found

        logical(c_bool) :: newmem = .false.
        logical(c_bool) :: cfound

        if (.not. obj%wasInit) then
        !if (.not. c_associated(obj%qobj)) then
            call qtreetbl_objinit(obj)
        end if

        call qtreetbl_find_nearest_c(self%q_cp, name, namesize, newmem, obj%qobj, cfound)
        if (cfound) then
            if (present(found)) found = .true.
        else
            if (present(found)) found = .false.
        end if
    end subroutine

    !> @brief Get current size (number of pairs) in the container.
    !!
    integer function qtreetbl_size(self)
        class(qtreetbl_t), intent(in) :: self

        qtreetbl_size = int(qtreetbl_size_c(self%q_cp), kind=kind(1))
    end function

    !> @brief Remove all values from the tree.
    !!
    subroutine qtreetbl_clear(self)
        class(qtreetbl_t), intent(inout) :: self

        call qtreetbl_clear_c(self%q_cp)
    end subroutine

    !> @brief Copy procedure.
    !!
    subroutine qtreetbl_copy(to, from)
        type(qtreetbl_t), intent(inout) :: to
        type(qtreetbl_t), intent(in)    :: from

        if (.not. to%was_init) then
            to%q_cp = c_null_ptr
            to%was_init = .true.
        end if
        call qtreetbl_copy_c(to%q_cp, from%q_cp)
        to%size_data = from%size_data
    end subroutine



    !**********************************************
    !*** Procedures for work with qtreetbl_obj_t
    !**********************************************

    !> @brief Initialize the object.
    !!
    subroutine qtreetbl_objinit(obj)
        class(qtreetbl_obj_t), intent(inout) :: obj

        if (.not. obj%wasInit) then
            obj%wasInit = .true.
            obj%qobj = c_null_ptr
        end if

        if ( .not. c_associated(obj%qobj)) then
            obj%qobj = qtreetbl_getobj_c()
        else
            call qtreetbl_objinit_c(obj%qobj)
        end if
    end subroutine

    !> @brief Destructor.
    !!
    subroutine qtreetbl_objfree(obj)
        type(qtreetbl_obj_t), intent(inout) :: obj

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
    subroutine qtreetbl_obj_copy(to, from)
        type(qtreetbl_obj_t), intent(inout) :: to
        type(qtreetbl_obj_t), intent(in)    :: from

        print *, "Error. qtreetbl_obj_t object can not be copied."
        print *, "Sub: qtreetbl_obj_copy"
        stop "Program terminated!"
    end subroutine

    !> @brief Get data from the object.
    !!
    subroutine qtreetbl_getdata(obj, val_data)
        class(qtreetbl_obj_t) :: obj
        include "nocheck_val_data.fi"
        real, dimension(*) :: val_data    !< returned data

        call qtreetbl_getdata_c(obj%qobj, val_data)
    end subroutine

    !> @brief Get name from the object.
    !!
    subroutine qtreetbl_getname(obj, name)
        class(qtreetbl_obj_t) :: obj
        character(len=:), allocatable, intent(inout) :: name  !< returned name

        integer(c_size_t) :: namesize

        namesize = qtreetbl_getnamesize_c(obj%qobj)
        if (allocated(name)) then
            if (len(name) /= namesize) then
                deallocate(name)
                allocate(character(len=namesize) :: name)
            end if
        else
            allocate(character(len=namesize) :: name)
        end if

        call qtreetbl_getname_c(obj%qobj, name)
    end subroutine

    !> @brief Get name from the object.
    !!
    subroutine qtreetbl_getname_by_obj(obj, name)
        class(qtreetbl_obj_t) :: obj
        include "nocheck_name.fi"
        real, dimension(*), intent(inout) :: name   !< returned name

        call qtreetbl_getname_c(obj%qobj, name)
    end subroutine

end module qtreetbl_m
