! Function returns multiple results
program swapper
    implicit none
    character(len=5), dimension(2) :: swapped

    swapped = swap("hello","world")
    print *, swapped(1), " ", swapped(2)

    contains

        function swap(left, right) result (rightleft)
            character(len=5), intent(in) :: left,right
            character(len=5), dimension(2) :: rightleft
            rightleft(1) = right
            rightleft(2) = left
        end function swap

end program swapper