program main_kadane_sequential
  use Homework
  real(8), allocatable :: A (:,:)
  integer(4) :: x1, y1, x2, y2, i ,j, N, M
  real(8) :: r
  write(*,*) "N, M = ?"
  read(*,*) N, M
  allocate(A(N,M))

  call random_seed
  do i = 1, N
    do j = 1, M
      call random_number(r)
      A(i,j) = r * 100-50
      write(*,"(F5.1,A)", advance="no") A(i,j), " "
    end do
    write(*,*)
  end do
  call FindMaxCoordinates(A, x1, y1, x2, y2)
  write (*,"(A,I3,A,I3,A,I3,A,I3,A)") "(",x1, ",",y1, ") (",x2, ",",y2,")"
end program
