program regex_test
    use regex
    logical match
    integer n, status, matches(2,10)
    type(regex_t) :: r

    character(len=*), parameter :: pattern = "([0-9]+)+"
    character(len=*), parameter :: target = "100 10 5 2 1 a b c"

    n=10
    call regex_compile(r, pattern, 'xm')
    match=regex_exec(r, target, matches)
    do i=1,n
        if (matches(1,i) == NOMATCH) exit
        print *, 'match="',regex_match(i, target, matches),'"'
    enddo
    call regex_free(r)
end program regex_test