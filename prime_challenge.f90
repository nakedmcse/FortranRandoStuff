program prime_challenge
    implicit none
    integer(kind=16), dimension(9) :: primes_before_list
    integer(kind=16) :: given
    real :: start_time, end_time
    integer :: i

    primes_before_list(1) = 60_16
    primes_before_list(2) = 6000000000
    primes_before_list(3) = 60000000000
    primes_before_list(4) = 2_16 ** 32_16
    primes_before_list(5) = 2_16 ** 40_16
    primes_before_list(6) = 2_16 ** 44_16
    primes_before_list(7) = 2_16 ** 48_16
    primes_before_list(8) = 2_16 ** 56_16
    primes_before_list(9) = 2_16 ** 64_16

    do i = 1, 9
        if (mod(primes_before_list(i),2) == 0) then
            given = primes_before_list(i)-1
        else
            given = primes_before_list(i)-2
        end if

        call cpu_time(start_time)
        do while(given > 0 .and. .not. check_prime(given))
            given = given - 2
        end do
        call cpu_time(end_time)

        if (given > 0) then
            print *, "First prime before ", primes_before_list(i), " is ", given, " in ", real(end_time - start_time)*1000
        else
            print *, "No previous prime found in ", real(end_time - start_time)*1000
        end if
    end do

    contains

        function check_prime(given) result(res)
            integer(kind=16) :: given
            integer :: i, max
            logical :: res

            res = .true.
            if(given == 1 .or. mod(given, 2) == 0) then
                res = .false.
                return
            elseif(given < 4) then
                res = .true.
                return
            else
                max = int(sqrt(real(given, kind=16)))
                do i = 3, max, 2
                    if (mod(given, i) == 0) then
                        res = .false.
                        exit
                    end if
                end do
            end if
        end function check_prime
end program prime_challenge