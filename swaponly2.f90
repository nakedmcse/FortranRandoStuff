program swap_only_2
    implicit none
    integer :: a, b

    a = 2
    b = 5
    print *, "Initial a,b ", a, b

    a = a + b  ! Add both
    b = a - b  ! b then becomes a - b (IE original a)
    a = a - b  ! a then becomes a - b (IE original b)

    print *, "Final a,b ", a, b
end program swap_only_2