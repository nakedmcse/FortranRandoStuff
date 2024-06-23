program dsafun
    implicit none

    type listentry
        integer :: value
        type(listentry), pointer :: next
    end type listentry

    type(listentry), pointer :: list
    integer :: i

    allocate(list)
    list%value = 0
    list%next => null()

    do i = 1, 9
        call append(list, i)
    end do

    call dump(list)
    call reverse(list)
    call dump(list)

    contains

        subroutine append(listhead, value)
            integer :: value
            type(listentry), pointer :: listhead, current, newnode

            allocate(newnode)
            newnode%value = value
            newnode%next => null()

            current => listhead

            do while (associated(current%next))
                current => current%next
            end do
            current%next => newnode
        end subroutine append

        subroutine dump(listhead)
            type(listentry), pointer :: listhead, current
            character(len=:), allocatable :: output
            character(len=4) :: stringval

            output = ""
            current => listhead
            do while (associated(current))
                write(stringval, '(I4)') current%value
                output = output // trim(adjustl(stringval)) // ","
                current => current%next
            end do
            print *, output(:len_trim(output)-1)
            deallocate(output)
        end subroutine dump

        subroutine reverse(listhead)
            type(listentry), pointer :: listhead, current, previous, next

            current => listhead
            previous => null()

            do while (associated(current))
                next => current%next
                current%next => previous
                previous => current
                current => next
            end do

            listhead => previous
        end subroutine reverse
end program dsafun