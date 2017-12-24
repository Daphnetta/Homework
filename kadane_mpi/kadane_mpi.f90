module Homework
  use mpi
  contains
    subroutine FindMaxCoordinates(A, x1, y1, x2, y2)
      real(8), intent(in), dimension(:,:) :: A
      integer(4), intent(out) :: x1, y1, x2, y2
      real(8), allocatable :: p (:), maximums (:)
      integer(4) :: h, g, i, j, N, M
      real(8) :: maximum, temporary_sum
      integer(4) :: mpi_master, mpi_rank, mpi_size, mpi_err, mpi_correct

      N = size(A, 1); M = size(A, 2)
      allocate(p(M))

      mpi_master = 0
      call mpi_comm_rank(mpi_comm_world, mpi_rank, mpi_err)
      call mpi_comm_size(mpi_comm_world, mpi_size, mpi_err)
      allocate(maximums(mpi_size))

      maximum = A(1,1); x1 = 1; y1 = 1; x2 = 1; y2 = 1
      do g = 1, N
         if (mod(g, mpi_size) == mpi_rank) then
            do j = 1, M
               p(j) = 0
            end do
            do i = g, N
               temporary_sum = 0; h = 1
               do j = 1 , M
                  p(j) = p(j) + A(i,j); temporary_sum = temporary_sum+p(j)
                  if (temporary_sum > maximum) then
                     maximum = temporary_sum; x1 = g; y1 = h; x2 = i; y2 = j
                  else if (temporary_sum <= 0) then
                     temporary_sum = 0; h = j + 1
                  end if
               end do
            end do
         end if
      end do
      call mpi_allgather(maximum, 1, mpi_real8, maximums, 1, mpi_real8, mpi_comm_world, mpi_err)
      mpi_correct = maxloc(maximums, 1) - 1
      call mpi_bcast(x1, 1, mpi_real8, mpi_correct, mpi_comm_world, mpi_err)
      call mpi_bcast(y1, 1, mpi_real8, mpi_correct, mpi_comm_world, mpi_err)
      call mpi_bcast(x2, 1, mpi_real8, mpi_correct, mpi_comm_world, mpi_err)
      call mpi_bcast(y2, 1, mpi_real8, mpi_correct, mpi_comm_world, mpi_err)
    end subroutine
end module
