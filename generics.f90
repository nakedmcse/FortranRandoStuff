!Looking at generics in FORTRAN
program generics
    implicit none

    type string_type
        character(:),allocatable :: a
        character(:),allocatable :: b
    end type string_type

    type int_type
        integer :: a
        integer :: b
    end type int_type

    type(string_type) :: stringsObj
    type(int_type) :: intsObj

    stringsObj%a = "One"
    stringsObj%b = "Two"

    intsObj%a = 1
    intsObj%b = 2

    print *, doSomething(stringsObj)
    print *, doSomething(intsObj)

    contains

        function handle_int_type(input) result (output)
            type(int_type), intent(in) :: input
            integer :: output
            output = input%a + input%b
        end function handle_int_type

        function handle_string_type(input) result (output)
            type(string_type), intent(in) :: input
            character(:), allocatable :: output
            output = trim(input%a) // ' plus ' // trim(input%b)
        end function handle_string_type

        function doSomething(T) result (output)
            class(*) T
            character(len=2) :: buffer
            character(:), allocatable :: output
            select type(T)
                type is (int_type)
                    write(buffer,'(i1)') handle_int_type(T)
                    output = trim(buffer)
                type is (string_type)
                    output = handle_string_type(T)
            endselect
        end function doSomething
end program generics