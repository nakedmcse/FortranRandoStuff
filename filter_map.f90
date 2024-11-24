program filter_map
    implicit none
    integer, dimension(10) :: numbers = (/1,2,3,4,5,6,7,8,9,10/)
    integer, dimension(5) :: outnumbers
    integer :: i = 0, o = 1

    do i = 1, 10
        if(mod(numbers(i), 2) == 0) then
            outnumbers(o) = numbers(i)**2
            o = o + 1
        end if
    end do

    do i = 1, 5
        print *, outnumbers(i)
    end do
end program filter_map