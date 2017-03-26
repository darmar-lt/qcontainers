
program test_qtreetbl
    use qtreetbl_m
    implicit none

    type price_t
        integer :: eur
        integer :: cnt
    end type

    type(qtreetbl_t)     :: qt
    type(price_t)        :: pval, pback
    type(qtreetbl_obj_t) :: tobj
    integer :: siz
    logical :: found
    character(len=:), allocatable :: key

    ! Storage size in bytes
    siz = storage_size(pval) / 8

    ! Initialize container
    call qt%new(siz)

    ! Put some values
    pval%eur = 1
    pval%cnt = 25
    call qt%put("Apples", pval)

    pval%eur = 0
    pval%cnt = 85
    call qt%put("Milk", pval)

    pval%eur = 1
    pval%cnt = 19
    call qt%put("Bananas", pval)

    !----------------
    ! Get values back

    !Iterate over all keys
    call tobj%init() ! should be initialized before the use
    do while(qt%getnext(tobj))
        call tobj%getname(key)
        call tobj%getdata(pback)

        print *, key, ": ", pback%eur, " Eur ", pback%cnt, " cent"
    end do

    call qt%get("Oranges", pback, found)
    if (found) then
        print *, "Oranges: ", pback%eur, " Eur ", pback%cnt, " cent"
    else
        print *, "No oranges today :("
    end if

    !------------
    ! Update value
    pval%eur = 1
    pval%cnt = 9
    call qt%put("Milk", pval)

    ! Get back
    call qt%get("Milk", pback, found)
    print *, "New price for milk: ", pback%eur, " Eur ", pback%cnt, " cent"

end program

