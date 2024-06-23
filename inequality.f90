! Stupid Fortran Tricks
#define a pp()
program inequality
    implicit none
    integer :: b
    b = 1

    if ( a == 1 .and. a == 2 .and. a == 3 ) print *,'Yes, It does'

    contains

    function pp() result (res)
        integer :: res
        res = b
        b = b + 1
    end function pp
end program inequality