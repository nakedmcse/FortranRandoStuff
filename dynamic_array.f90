! Dynamic Array
program dynamic_array
    implicit none
    integer :: i

    type vector
        integer, dimension(:), allocatable :: items
        integer :: count
    end type vector

    type(vector) :: dynArray

    allocate(dynArray%items(1))
    dynArray%count = 0

    do i = 1, 100000
        call append(dynArray, i)
    end do

    print *, dynArray%items(1)
    print *, dynArray%items(50000)
    print *, dynArray%items(100000)

    contains

        subroutine append(array, value)
            integer :: value
            integer, dimension(:), allocatable :: temp
            type(vector) :: array

            if(size(array%items) - array%count == 0) then
                allocate(temp(array%count * 2))
                temp(1:array%count) = array%items
                deallocate(array%items)
                allocate(array%items(array%count * 2))
                array%items = temp
                deallocate(temp)
            end if

            array%count = array%count + 1
            array%items(array%count) = value
        end subroutine append
end program dynamic_array