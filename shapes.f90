module shape
    implicit none

    type, public :: Circle
        real :: radius
    end type Circle

    type, public :: Rectangle
        real :: length, width
    end type Rectangle

    interface area
        module procedure area_circle
        module procedure area_rectangle
    end interface area

    contains
        real function area_circle(x)
            type(Circle), intent(in) :: x
            real :: pi = 4.0 * atan(1.0)
            area_circle = pi * (x%radius * x%radius)
        end function area_circle

        real function area_rectangle(x)
            type(Rectangle), intent(in) :: x
            area_rectangle = x%length * x%width
        end function area_rectangle
end module shape

program shapes
    use shape
    implicit none
    type(Circle) :: C = Circle(radius = 5)
    type(Rectangle) :: R = Rectangle(length = 5, width = 10)
    print *, 'Circle area is', area(C)
    print *, 'Rectangle area is', area(R)
end program shapes