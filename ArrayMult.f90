program array_mult
    implicit none
    real :: start_time, end_time
    integer, dimension(1:100000000) :: a, b, c, d
    integer :: i

    do i=1, 100000000
        a(i) = i
        b(i) = i
        c(i) = 0
    end do

    call cpu_time(start_time)
    c = a * b
    call cpu_time(end_time)

    print *, c(567)
    print *, "Mult Completed in:", real(end_time - start_time)*1000
end program array_mult