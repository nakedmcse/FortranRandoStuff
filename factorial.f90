program factorial
    implicit none
    integer :: n, result
    n = 10
    print *, "Factorial 100 recursive:", factorial_recurse(n)

    contains
        recursive function factorial_recurse(n) result (fact)
            integer, intent(in) :: n
            integer :: fact

            if (n == 0) then
                fact = 1
            else
                fact = n * factorial_recurse(n - 1)
            end if
        end function factorial_recurse
end program factorial