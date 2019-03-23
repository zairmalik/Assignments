program main
  implicit none
  real :: f
  real :: x1 = 0.0, x2 = 1.0
  real :: y = 0
  integer :: i

  if (f(x1)*f(x2) >= 0) then
    print *, "Invalid intervals assumed."
  end if

  do i = 0, 9
    y = (x1*f(x2) - x2*f(x1))/(f(x2) - f(x1))

    if (f(y) == 0) then
        exit
    else if (f(y)*f(x1) < 0) then
        x2 = y
    else
        x1 = y
    end if
  end do

  print *, "Function: cos(x) - x^3"
  print *, "Number of iteration: 10"
  print *, "Root of equation: ", y
end

real function f(x)
    implicit none
    real :: x

    f = cos(x) - x**3
end function
