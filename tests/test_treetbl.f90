
subroutine test_qtreetbl_1()
    use qtreetbl_m
    use assert_test_m
    implicit none

    type(qtreetbl_t) :: qt
    integer :: isiz
    integer :: v, vg
    logical :: suc, haskey
    logical :: isok
    character(len=20) :: key
    type(qtreetbl_obj_t) :: obj

    print *, ""
    print *, "Start of test_qtreetbl_1"

    isiz = storage_size(v) / 8
    key = "namas"
    call qt%new(isiz)
    v = 101
    call qt%put("e1", v)
    v = 102
    call qt%put("e2", v, success=suc)
    ! Overwrite key
    v = 1020001
    call qt%put("e2", v)
    v = 103
    call qt%put("e3", v)
    v = 104
    call qt%put("e4", v)
    v = 105
    call qt%put("e5", v)

    !--------------
    call qt%get("e2", vg, haskey)
    call assert_true(haskey)      ! should be found
    call assert_equal(vg, 1020001)

    !--------------
    call qt%get("something", vg, haskey)
    call assert_false(haskey) ! should not be found

    !--------------
    isiz = qt%size()
    call assert_equal(isiz, 5)

    !--------------
    call obj%init()
    do while(qt%getnext(obj))
        key = "" ! empty name
        call obj%getname_by_obj(key) ! key should be big enough
        print *, "getnext, key=", trim(key)
        if (key == "e4") then
            vg = -1
            call obj%getdata(vg)
            call assert_equal(vg, 104)
        end if
    end do

    !--------------
    call qt%remove("e2", isok)
    call assert_true(isok)
    isiz = qt%size()
    call assert_equal(isiz, 4)

    !--------------
    call qt%clear()
    isiz = qt%size()
    call assert_equal(isiz, 0)
end subroutine

subroutine test_qtreetbl_2()
    use qtreetbl_m
    use assert_test_m
    implicit none

    type(qtreetbl_t) :: qt, qt_copy
    character(len=80) :: chv
    character(len=:), allocatable :: chv_back
    logical :: haskey
    integer :: i

    print *, ""
    print *, "Start of test_qtreetbl_2"

    call qt%new()
    chv = "Santa"
    call qt%putstr("e1", trim(chv))
    chv = "Claus II"
    call qt%putstr("e2", trim(chv))
    chv = "Elf"
    call qt%putstr("e3", trim(chv))
    chv = "Snow"
    call qt%put_by_obj("e4", 2, trim(chv), len_trim(chv))

    !---------
    call qt%getstr("e2", chv_back, haskey)
    call assert_true(haskey)
    call assert_equal(chv_back, "Claus II    ")

    !----------
    call qt%getstr("something", chv_back, haskey)
    call assert_false(haskey)

    !----------
    haskey = qt%haskey("eee")
    call assert_false(haskey)

    !----------
    haskey = qt%haskey_by_obj("e4", 2)
    call assert_true(haskey)

    !----------
    i = qt%size()
    qt_copy = qt
    call qt%clear()

    call assert_equal(qt%size(), 0)
    call assert_equal(qt_copy%size(), i)

    haskey = qt_copy%haskey("e2")
    call assert_true(haskey)
end subroutine

subroutine testing_qtreetbl()
    use assert_test_m
    implicit none

    print *, "Testing treetbl"

    call assert_init()

    call test_qtreetbl_1()
    call test_qtreetbl_2()

    call assert_print_summary()
end subroutine

