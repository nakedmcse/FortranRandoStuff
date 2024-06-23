program for_collatz
    implicit none

    type memo_entry
        integer seed
        integer steps
    end type memo_entry

    type(memo_entry), dimension(100000000) :: memos
    integer :: seed, steps
    real :: start_time, end_time

    call cpu_time(start_time)
    do seed=0,1000000000
        steps = collatz(seed,memos)
        memos(mod(seed,100000000)+1)%seed = seed
        memos(mod(seed,100000000)+1)%steps = steps
        if(mod(seed,100000000) == 0) then
            print *, "Seed:", seed, "Steps:", steps
        end if
    end do
    call cpu_time(end_time)
    print *, "Collatz Completed in:", real(end_time - start_time)*1000

    contains
        function collatz(oseed,memos) result (csteps)
            integer, intent(in) :: oseed
            type(memo_entry), dimension(100000000) :: memos
            integer :: csteps, cseed
            cseed = oseed
            csteps = 0
            do while(cseed > 1)
                if (memos(mod(cseed,100000000)+1)%seed == cseed) then
                    csteps = csteps + memos(mod(cseed,100000000)+1)%steps
                    exit
                end if
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