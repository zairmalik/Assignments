program main
  implicit none
  real :: epsilon = 0.001 !threshold for verifying roots exist.
  real :: original_function, derivative_function !functions
  real :: h, x

  print *, "Enter an initial guess."
  read *, x

  do while (abs(h) .ge. epsilon)
    h = original_function(x)/derivative_function(x)
    x = x - h
  end do

  print *, "The root of the equation is ", x
end

real function original_function(x_initial)
    implicit none
    real :: x_initial

    original_function = x_initial**3+2*x_initial+9
end function

real function derivative_function(x_initial)
    implicit none
    real :: x_initial

    derivative_function = 3*x_initial**2+2
end function
