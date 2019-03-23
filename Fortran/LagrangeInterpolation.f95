program main
  implicit none
  real :: original_f, L_f;
  real :: x, x_f, x_L

  print *, "Lagrange interpolation for the f(x)=x^5"
  print *, "Enter x."
  read *, x

  x_f = original_f(x)
  x_L = L_f(x)

  if (x_L .ne. x_f) then
    print *, "The interpolation only works for x=1, 2, 3, 4 and 5."
  end if

  print *, "Original function result: ", x_f
  print *, "Lagrange interpolation of the original function: ", x_L
end

real function original_f(x)
    real :: x

    original_f = x**5
end function

real function L_f(x)
    real :: x

    L_f = 15*x**4-85*x**3+225*x**2-274*x+120
end function
