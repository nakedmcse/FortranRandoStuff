program for_collatz
    implicit none
    integer :: seed, steps

    do seed=0,1000000000
        steps = collatz(seed)
        if(mod(seed,1000000) == 0) then
            print *, "Seed:", seed, "Steps:", collatz(seed)
        end if
    end do

    contains
        function collatz(oseed) result (csteps)
            integer, intent(in) :: oseed
            integer :: csteps, cseed
            cseed = oseed
            csteps = 0
            do while(cseed > 1)
                do while(mod(cseed,2) == 0)
                    csteps = csteps + 1
                    cseed = cseed / 2
                end do
                if(cseed > 1) then
                    csteps = csteps + 1
                    cseed = cseed * 3 + 1
                end if
            end do
        end function collatz
end program for_collatz