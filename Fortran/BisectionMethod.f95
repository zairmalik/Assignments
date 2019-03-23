program main
  implicit none
  real :: f
  real :: delta = 1.0E-4
  real :: u1_i = 1.0, u2_i = 2.0
  real :: u1, u2, u3, temp_u3, x
  integer :: i = 0

  u1 = u1_i
  x = f(u1)
  u2 = u2_i
  x = f(u2)

  if (f(u1)*f(u2) >= 0) then
    print *, "Your initial value is invalid."
    print *, "The root isn't located in the initial values."
    stop
  end if

  u3 = (u1 + u2)/2.0
  temp_u3 = u3

  do
    if (f(u1)*f(u3) < 0) then
        u2 = u3
        u3 = (u1 + u2)/2.0
    else if (f(u2)*f(u3) < 0) then
        u1 = u3
        u3 = (u1 + u2)/2.0
    end if

    x = abs((temp_u3 - u3)/temp_u3)
    i = i + 1

    if (x <= delta) then
        exit
    end if

    temp_u3 = u3
  end do

  print *, "Function: x^3-x-2"
  print *, "Total iterations: ", i
  print *, "Final root value: ", u3
end

real function f(x)
    implicit none
    real :: x

    f = x**3-x-2
end function
