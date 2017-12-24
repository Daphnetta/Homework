module Homework
  contains
    subroutine FindMaxCoordinates(A, x1, y1, x2, y2)
      real(8), intent(in), dimension(:,:) :: A
      integer(4), intent(out) :: x1, y1, x2, y2
      real(8), allocatable :: p (:)
      integer(4) :: h, g, i, j, N, M
      real(8) :: maximum, temporary_sum
      N = size(A, 1); M = size(A, 2)
      allocate(p(M))

      maximum = A(1,1); x1 = 1; y1 = 1; x2 = 1; y2 = 1
      do g = 1, N
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
      end do
    end subroutine
end module
