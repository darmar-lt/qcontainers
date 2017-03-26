
program test_qcontainers
    use assert_test_m

    call assert_new(.true., .true.)

    call testing_qtreetbl()
    call testing_qhashtbl()
    call testing_qvector()
    call testing_qlist()

    call assert_conclude()
end program
