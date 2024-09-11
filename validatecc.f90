program validatecc
    implicit none

    print *, "4137 8947 1175 5904:", validate_cc("4137 8947 1175 5904")
    print *, "1234 5678 1234 5678:", validate_cc("1234 5678 1234 5678")

    contains

    function validate_cc(cardnumber) result (res)
        character(len=*) :: cardnumber
        logical :: res
        integer :: sum, i, val

        sum = 0
        do i = 1, len_trim(cardnumber)
            if (cardnumber(i:i) /= ' ') then
                read(cardnumber(i:i),*) val
                val = merge(merge((val * 2)-9, val * 2, val * 2 > 9), val, mod(i,2) == 0)
                sum = sum + val
            end if
        end do
        res = (mod(sum,10) == 0)
    end function validate_cc
end program validatecc