
program test_qlist
    use qlist_m
    implicit none

    type(qlist_t)     :: ql
    integer :: arr(2,10)
    integer :: bvec(2)
    integer :: siz, i

    ! Fill values into array
    arr = reshape([(i,i=1,size(arr))], shape(arr))

    ! Storage size for TWO array elements in bytes
    siz = storage_size(arr) / 8 * 2

    ! Initialize the list
    call ql%new(siz)

    ! Add every second column of arr into the list
    do i = 1, 10, 2
        call ql%addlast(arr(1,i))
    end do

    ! Look into the list, if we have there, what we are expecting.
    do i = 1, ql%size()
        call ql%getat(i, bvec(1))
        print *, "In i=", i, " element list has ", bvec
    end do

end program

