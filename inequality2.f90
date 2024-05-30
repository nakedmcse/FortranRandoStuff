module intmod
    type int
        integer :: val
    end type int
end module intmod

function cmp(x,y) result (res)
    use intmod
    implicit none
    type(int), intent(in) :: x
    integer, intent(in) :: y
    logical :: res
    res = .true.
end function cmp

program inequality2
    use intmod
    implicit none

    interface operator (==)
        function cmp(x,y) result (res)
            use intmod
            type(int), intent(in) :: x
            integer, intent(in) :: y
            logical :: res
        end function cmp
    end interface

    type(int) :: a
    a%val = 1

    if (a == 1 .and. a == 2 .and. a == 3) then
       print *, 'Yes, a equals 1, 2 and 3 !'
    end if
end program inequality2