program main
  implicit none
  integer :: asc_numbers(5)
  integer :: dsc_numbers(5)
  integer :: i, j
  integer :: numbers

  print *, "Enter the numbers."

  do i=1, 5
    read *, asc_numbers(i)
  end do

  do i=1, 5
    dsc_numbers(i) = asc_numbers(i)
  end do

  do i=1, 5
    do j=i, 5
        if (asc_numbers(i) > asc_numbers(j)) then
            numbers = asc_numbers(j)
            asc_numbers(j) = asc_numbers(i)
            asc_numbers(i) = numbers
        end if
    end do
  end do

  print *, "The numbers in increasing order is:"

  do i=1, 5
    print *, asc_numbers(i)
  end do

  do i=1, 5
    do j=i, 5
        if (dsc_numbers(i) < dsc_numbers(j)) then
            numbers = dsc_numbers(j)
            dsc_numbers(j) = dsc_numbers(i)
            dsc_numbers(i) = numbers
        end if
    end do
  end do

  print *, "The numbers in decreasing order is:"

  do i=1, 5
    print *, dsc_numbers(i)
  end do
end
