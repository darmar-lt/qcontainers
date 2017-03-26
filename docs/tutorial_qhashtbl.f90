
program test_qhashtbl
    use qhashtbl_m
    use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
    implicit none

    type value_t
        integer :: nv
        integer, allocatable :: val(:)
    end type

    type(qhashtbl_t)     :: qh
    type(qhashtbl_obj_t) :: hobj
    type(value_t), pointer :: pval, pback
    integer :: siz, i, j
    logical :: found
    character(len=6) :: names = "abcdef"
    type(c_ptr) :: cp


    ! Storage size in bytes
    siz = storage_size(cp) / 8

    ! Initialize the container
    ! 'range' value defines the size of table is used internally.
    ! It is recommended to use a value between (total_number_of_keys / 3) ~ (total_number_of_keys * 2)
    call qh%new(range=10, size_data=siz)


    ! Put some values
    do i = 1, 6
        allocate(pval)
        allocate(pval%val(i))
        pval%nv = i
        pval%val(:) = [(j,j=1,i)]
        cp = c_loc(pval)

        call qh%put(names(i:i), cp)
    end do

    ! Take some values back
    call qh%get(names(2:2), cp, found)
    if (found) then
        call c_f_pointer(cp, pback)
        print *, "'", names(2:2), "' has values nv=", pback%nv, " val=", pback%val
    end if

    call qh%get(names(5:5), cp, found)
    if (found) then
        call c_f_pointer(cp, pback)
        print *, "'", names(5:5), "' has values nv=", pback%nv, " val=", pback%val
    end if

    ! We allocated memory for pointers.
    ! Now we have to free this memory.
    call hobj%init()
    do while(qh%getnext(hobj))
        call hobj%getdata(cp)
        call c_f_pointer(cp, pback)
        print *, "Value will be deallocated: nv=", pback%nv, " val=", pback%val
        deallocate(pback)
    end do

    ! Now C pointers stored in the container point to the freed locations.

end program

