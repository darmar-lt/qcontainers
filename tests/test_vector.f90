subroutine test_qvector_1()
    use assert_test_m
    use qvector_m
    implicit none

    type(qvector_t) :: vec
    integer :: isiz
    real :: v, vb
    integer :: i
    logical :: success

    print *, ""
    print *, "Start of test_qvector_1"

    v = 1.
    isiz = storage_size(v) / 8
    call vec%new(10, isiz, QVECTOR_RESIZE_LINEAR)

    do i = 1, 5
        v = i
        call vec%addlast(v)
    end do

    call vec%addat(7, v, success) ! WRONG. No empty elements can be left.
    call assert_false(success)

    v = 50.
    call vec%setat(5, v)

    call vec%getfirst(vb)
    call assert_equal(vb, 1.)

    call vec%getlast(vb)
    call assert_equal(vb, 50.)

    call vec%getat(3, vb, success)
    call assert_equal(vb, 3.)
    call assert_true(success)

    call vec%getat(8, vb, success) ! WRONG.
    call assert_false(success)

    vb = 99.
    call vec%popfirst(vb)
    call assert_equal(vb, 1.)
    call vec%getat(2, vb)
    call assert_equal(vb, 3.)

    call vec%removeat(1, success)
    call assert_true(success)
    vb = 99.
    call vec%getfirst(vb)
    call assert_equal(vb, 3.)

    i = vec%size()
    call assert_equal(i, 3)

    call vec%resize(2, success)
    call assert_true(success)
    i = vec%size()
    call assert_equal(i, 2)
    vb = 99.
    call vec%getat(2, vb, success)
    call assert_true(success)
    call assert_equal(vb, 4.)
end subroutine

subroutine test_qvector_2()
    use assert_test_m
    use qvector_m
    implicit none

    type(qvector_t) :: vec
    integer :: isiz
    real(8) :: v, vb
    integer :: i
    real(8), allocatable :: varr(:)
    type(qvector_obj_t) :: vobj

    print *, ""
    print *, "Start of test_qvector_2"

    v = 1.
    isiz = storage_size(v) / 8
    call vec%new(2, isiz, QVECTOR_RESIZE_LINEAR)

    do i = 1, 5
        v = i
        call vec%addlast(v)
    end do

    v = 301
    call vec%setat(3, v)

    i = vec%size()
    call assert_equal(i, 5)
    allocate(varr(i))
    call vec%toarray(varr)

    do i = 1, size(varr)
        if (i == 3) then
            call assert_equal(real(301,kind=8), varr(i))
        else
            call assert_equal(real(i,kind=8), varr(i))
        end if
    end do

    v = 3
    call vec%setat(3, v)
    call vec%reverse()

    call vobj%init()
    i = 5
    do while (vec%getnext(vobj))
        call vobj%getdata(vb)
        call assert_equal(vb, real(i,kind=8))
        i = i - 1
    end do

    call vec%removeat(3)
    i = vec%size()
    call assert_equal(i, 4)

    call vec%clear()
    i = vec%size()
    call assert_equal(i, 0)
end subroutine

subroutine test_qvector_3()
    use assert_test_m
    use qvector_m
    implicit none

    type(qvector_t) :: vec, vec_copy
    integer :: isiz
    integer :: i
    logical :: success

    print *, ""
    print *, "Start of test_qvector_3"

    isiz = storage_size(i) / 8
    call vec%new(2, isiz, QVECTOR_RESIZE_EXACT)

    do i = 1, 5
        call vec%addlast(i)
    end do

    vec_copy = vec
    call vec_copy%addlast(6)
    call vec_copy%setat(1, -1, success)
    call assert_true(success)

    call vec_copy%getfirst(i)
    call assert_equal(i, -1)

    i = vec_copy%size()
    call assert_equal(i, 6)

    i = vec%size()
    call assert_equal(i, 5)

    call vec%getlast(i, success)
    call assert_true(success)
    call assert_equal(i, 5)
end subroutine

subroutine testing_qvector()
    use assert_test_m
    implicit none

    print *, "Testing of qvector"
    call assert_init()

    call test_qvector_1()
    call test_qvector_2()
    call test_qvector_3()

    call assert_print_summary()
end subroutine

