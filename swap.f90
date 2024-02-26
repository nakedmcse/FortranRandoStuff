! Function returns multiple results
program swapper
    implicit none

    type values
        character(:),allocatable :: left
        character(:),allocatable :: right
    end type values

    type(values) :: swapped

    swapped%left = "hello"
    swapped%right = "world"
    swapped = swap(swapped);
    print *, swapped%left, " ", swapped%right

    contains

        function swap(input) result (output)
            type(values), intent(in) :: input
            type(values) :: output
            output%left = input%right
            output%right = input%left
        end function swap

end program swapper