! Dynamic Generic Array
program dynamic_generic_array
    implicit none

    type key_value
        character(len=:), allocatable :: key
        integer :: value = 0
        logical :: error = .false.
    end type key_value

    type dyn_array
        type(key_value), dimension(:), allocatable :: items
        integer :: count = 0
    end type dyn_array

    type(dyn_array) :: array
    type(key_value) :: kv
    integer :: i

    do i = 1,10
        kv%value = i
        kv%key = itoa(i)
        call append(array,kv)
    end do

    print *, 'KVs:'
    print *, array%items(1)%key, array%items(1)%value
    print *, array%items(5)%key, array%items(5)%value
    print *, array%items(10)%key, array%items(10)%value

    kv = pop(array)
    print *, 'Popped:'
    print *, kv%key, kv%value, kv%error

    contains

        subroutine append(this, value)
            type(dyn_array) :: this
            type(key_value) :: value
            type(key_value), dimension(:), allocatable :: temp

            if (.not. allocated(this%items)) then
                allocate(this%items(256))
            elseif (size(this%items) == this%count) then
                allocate(temp(this%count * 2))
                temp(1:this%count) = this%items(1:this%count)
                call move_alloc(temp,this%items)
            end if

            this%count = this%count + 1
            this%items(this%count) = value
        end subroutine append

        function pop(this) result (res)
            type(dyn_array) :: this
            type(key_value) :: res

            if (this%count == 0) then
                res%error = .true.
            else
                res = this%items(this%count)
                this%count = this%count - 1
            end if
        end function

        function itoa(i) result(res)
            integer, intent(in) :: i
            character(:), allocatable :: res
            character(range(i)+2) :: tmp

            write(tmp, '(I0)') i
            res = trim(tmp)
        end function itoa

end program dynamic_generic_array