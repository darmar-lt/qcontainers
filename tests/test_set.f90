
subroutine test_qset_1()
    use qset_m
    use assert_test_m
    implicit none

    type(qset_t) :: qs
    type(qset_obj_t) :: obj
    type(qset_t) :: qs_copy

    integer :: i, vback, iss
    integer, allocatable :: arr(:)
    integer, allocatable :: arr2(:)
    logical :: success

    print *, "qset test_1. Start"

    call qs%new(mold=i)
    do i = 1, 10
        call qs%put(i)
    end do

    call assert_true(qs%has(8))
    call assert_false(qs%has(0))

    !----------------------
    iss = qs%size()
    call assert_equal(iss, 10)
    allocate(arr(iss))
    arr = -999
    call obj%init()
    do while(qs%getnext(obj))
        call obj%getdata(vback)
        arr(vback) = vback
    end do

    do i = 1, 10
        call assert_equal(arr(i), i)
    end do
    deallocate(arr)

    !------------------
    ! Remove some value
    call qs%remove(vback, success)
    call assert_true(success)
    call qs%remove(vback, success)
    call assert_false(success)

    !-----------------------
    ! Put set values to array
    allocate(arr(qs%size()))
    arr = -999
    call qs%toarray(arr)
    print *, "Values from set: ", arr
    call assert_true(all(arr >= 1 .and. arr <= 10))

    !-----------------------
    ! Test copy
    qs_copy = qs
    call qs%clear()
    print *, "After clear size of set=", qs%size()

    allocate(arr2(qs_copy%size()))
    call qs_copy%toarray(arr2)
    print *, "Copied values=", arr2

    !--------------
    print *, "qset test_1. End"
end subroutine


subroutine test_qset_2()
    use qset_m
    use assert_test_m
    implicit none
    type(qset_t)     :: qs
    type(qset_obj_t) :: obj
    type(qset_t)     :: qs_copy

    integer :: i
    character(len=20) :: words(4)
    character(len=:), allocatable :: wback
    character(len=20) :: one_word
    logical :: success

    character(len=20), allocatable :: strarr(:)

    print *, "qset test_2. Start"

    words(1) = "Start"
    words(2) = "thinking."
    words(3) = "Use"
    words(4) = "Fortran."

    call qs%new(mold="a")  ! any character value can be used here
    do i = 1, size(words)
        call qs%putstr(words(i))
    end do

    call assert_true(qs%hasstr("Use"))
    call assert_false(qs%hasstr("use")) ! Upper and lower case are not the equal.

    !----------------------
    ! Iterate over the container
    print *, "Make logical thought from separate words:"
    call obj%init()
    do while(qs%getnext(obj))
        call obj%getdatastr(wback)
        print *, "wback=<", wback,">"
        call assert_true(any(wback == words(:)))
    end do

    !----------------------
    ! Test toarray
    allocate(strarr(qs%size()))
    call qs%toarraystr(strarr)
    print *, "Returned strarr="
    do i = 1, qs%size()
        print *, "<", strarr(i), ">"
        call assert_true(any(strarr(i) == words(:)))
    end do

    !----------------------
    ! Test remove string
    call assert_equal(qs%size(), 4)
    one_word = "Start"
    call qs%removestr(one_word, success)
    call assert_true(success)
    call assert_equal(qs%size(), 3)

    !---------------------
    ! Test copy
    qs_copy = qs
    call assert_equal(qs_copy%size(), 3)

    call obj%init()
    do while(qs_copy%getnext(obj))
        call obj%getdatastr(wback)
        print *, "Set copy wback=<", wback,">"
        call assert_true(any(wback == words(2:)))
    end do

    !--------------
    print *, "qset test_2. End"
end subroutine


subroutine testing_qset()
    use assert_test_m
    implicit none

    print *, "Testing of qset"

    call assert_init()

    call test_qset_1()
    call test_qset_2()

    call assert_print_summary()
end subroutine


