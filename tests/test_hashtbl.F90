
subroutine test_qhashtbl_1()
    use qhashtbl_m
    use assert_test_m
    implicit none

    type(qhashtbl_t) :: qh
    integer :: isiz
    integer :: v, vg, i
    logical :: haskey
    logical :: isok
    character(len=:), allocatable :: key
    type(qhashtbl_obj_t) :: obj

    print *, "qhashtbl test_1. Start"

    isiz = storage_size(v) / 8
    call qh%new(10, isiz)
    v = 101
    call qh%put("e1", v)
    v = 102
    call qh%put("e2", v)
    ! Overwrite key
    v = 1020001
    call qh%put("e2", v)
    v = 103
    call qh%put("e3", v)
    v = 104
    call qh%put("e4", v)
    v = 105
    call qh%put("e5", v)

    !--------------
    call qh%get("e2", vg, haskey)
    call assert_true(haskey)      ! should be found
    call assert_equal(vg, 1020001)

    !--------------
    call qh%get("something", vg, haskey)
    call assert_false(haskey) ! should not be found

    !--------------
    isiz = qh%size()
    call assert_equal(isiz, 5)

    !--------------
    call obj%init()
    do while(qh%getnext(obj))
        call obj%getname(key)
        print *, "getnext, key=", key
        if (key == "e4") then
            vg = -1
            call obj%getdata(vg)
            call assert_equal(vg, 104)
        end if
    end do

    !--------------
    i = 0
    call obj%init()
    do while(qh%getnext(obj))
        i = i + 1
    end do
    call assert_equal(i, 5)

    !--------------
    call qh%remove("e2", isok)
    call assert_true(isok)
    isiz = qh%size()
    call assert_equal(isiz, 4)

    !--------------
    call qh%clear()
    isiz = qh%size()
    call assert_equal(isiz, 0)

    print *, "qhashtbl test_1. End"

end subroutine

subroutine test_qhashtbl_2()
    use qhashtbl_m
    use assert_test_m
    implicit none

    type(qhashtbl_t) :: qh, qh_copy
    character(len=80) :: chv
    character(len=:), allocatable :: chv_back
    logical :: haskey

    print *, ""
    print *, "qhashtbl test_2. Start"

    call qh%new()
    chv = "Santa"
    call qh%putstr("e1", trim(chv))
    chv = "Claus II"
    call qh%putstr("e2", trim(chv))
    chv = "Elf"
    call qh%putstr("e3", trim(chv))
    chv = "Snow"
    call qh%put("e4", trim(chv), len_trim(chv))

    !---------
    call qh%getstr("e2", chv_back, haskey)
    call assert_true(haskey)
    call assert_equal(chv_back, "Claus II")

    !----------
    qh_copy = qh
    call assert_equal(qh_copy%size(), qh%size())
    chv = "Changed value for e4"
    call qh%putstr("e4", chv)
    call qh%getstr("e4", chv_back, haskey)
    call assert_equal(chv_back, chv)
    call qh_copy%getstr("e4", chv_back, haskey)
    call assert_equal(chv_back, "Snow")

    !----------
    call qh%getstr("something", chv_back, haskey)
    call assert_false(haskey)

    !----------
    haskey = qh%haskey("eee")
    call assert_false(haskey)

    print *, "qhashtbl test_2. Start"
end subroutine

#ifndef __PGI
subroutine test_qhashtbl_3()
    ! Warning: This test can't be compiled with PGI Fortran 16.10
    use iso_c_binding
    use qhashtbl_m
    use assert_test_m
    implicit none

    type(qhashtbl_t) :: qtable
    type(qhashtbl_t), pointer :: tab1
    type(c_ptr) :: tab1_cp
    integer :: v
    logical :: found

    print *, ""
    print *, "qhashtbl test_3. Start"

    call test_3a(qtable)

    call qtable%get("Table1", tab1_cp, found)
    call assert_true(found)

    call c_f_pointer(tab1_cp, tab1)

    call assert_equal(tab1%size(), 4)

    !--------------
    call tab1%get("e1", v, found)
    call assert_true(found)
    call assert_equal(v, 101)
    call tab1%get("e4", v, found)
    call assert_true(found)
    call assert_equal(v, 104)

    ! We allocated memory for the tab1 pointer in test_3a.
    ! Here we should free this memory:
    deallocate(tab1)

    print *, "qhashtbl test_3. End"

contains
    subroutine test_3a(qtbl_out)
        use qhashtbl_m
        use iso_c_binding
        use assert_test_m
        implicit none

        type(qhashtbl_t), intent(out) :: qtbl_out
        type(qhashtbl_t), pointer :: qtbl1
        type(c_ptr) :: qtbl1_cp
        integer :: v, isiz
        logical :: found

        print *, ""
        print *, "qhashtbl test_3a. Start"

        allocate(qtbl1)
        isiz = storage_size(v) / 8
        call qtbl1%new(5, isiz)
        call qtbl1%put("e1", 101)
        call qtbl1%put("e2", 102)
        call qtbl1%put("e3", 103)
        call qtbl1%put("e4", 104)

        call qtbl1%get("e2", v, found)

        call assert_true(found)
        call assert_equal(v, 102)

        isiz = storage_size(qtbl1_cp) / 8
        call qtbl_out%new(5, isiz)
        qtbl1_cp = c_loc(qtbl1)
        call qtbl_out%put("Table1", qtbl1_cp)

        print *, "qhashtbl test_3a. End"
    end subroutine test_3a
end subroutine test_qhashtbl_3
#endif

subroutine testing_qhashtbl()
    use assert_test_m
    implicit none

    print *, "Testing of qhashtbl"

    call assert_init()

    call test_qhashtbl_1()
    call test_qhashtbl_2()

#ifndef __PGI
    call test_qhashtbl_3()
#endif

    call assert_print_summary()
end subroutine

