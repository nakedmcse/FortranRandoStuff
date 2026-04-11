! Dynamic Generic Array
program dynamic_generic_array
    implicit none

    type key_value
        character(len=:), allocatable :: key
        class(*), allocatable :: value
        logical :: error = .false.
    end type key_value

    type dyn_array
        type(key_value), dimension(:), allocatable :: items
        integer :: count = 0
    end type dyn_array

    type(dyn_array) :: array_int, array_str
    type(key_value) :: kv_int, kv_str
    integer :: i

    do i = 1,10
        kv_int%value = i
        kv_int%key = itoa(i)
        call append(array_int,kv_int)

        kv_str%value = "string " // itoa(i)
        kv_str%key = itoa(i)
        call append(array_str,kv_str)
    end do

    print *, 'Int KVs:'
    print *, array_int%items(1)%key, unwrap_int(array_int%items(1)%value)
    print *, array_int%items(5)%key, unwrap_int(array_int%items(5)%value)
    print *, array_int%items(10)%key, unwrap_int(array_int%items(10)%value)

    kv_int = pop(array_int)
    print *, 'Int Popped:'
    print *, kv_int%key, unwrap_int(kv_int%value), kv_int%error
    print *,''

    print *, 'Str KVs:'
    print *, array_str%items(1)%key, "           ", unwrap_str(array_str%items(1)%value)
    print *, array_str%items(5)%key, "           ", unwrap_str(array_str%items(5)%value)
    print *, array_str%items(10)%key, "          ", unwrap_str(array_str%items(10)%value)

    kv_str = pop(array_str)
    print *, 'Str Popped:'
    print *, kv_str%key, "          ", unwrap_str(kv_str%value), kv_str%error

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

        function unwrap_int(value) result (res)
            integer :: res
            class(*) :: value
            select type(v => value)
                type is (integer)
                    res = v
                class default
                    res = -1
            end select
        end function unwrap_int

        function unwrap_str(value) result (res)
            character(len=:), allocatable :: res
            class(*) :: value
            select type(v => value)
            type is (character(*))
                res = v
            class default
                res = "unknown"
            end select
        end function unwrap_str

end program dynamic_generic_array