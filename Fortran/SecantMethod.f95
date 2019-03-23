program main
    implicit none
    real :: f
    real :: x0, x1 = 10.0, x2 = 30.0
    integer :: i

    do i = 0, 5
        x0 = (x1*f(x2) - x2*f(x1))/(f(x2) - f(x1))
        x1 = x2
        x2 = x0
    end do

    print *, "Function: x^2-612"
    print *, "Number of iteration: ", i
    print *, "Root of equation: ", x0
end

real function f(x)
    implicit none
    real :: x

    f = x**2-612
end function
