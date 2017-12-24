module Homework
  implicit none
  contains
    subroutine FindMaxCoordinates(A, x1, y1, x2, y2)
      real(8), allocatable, intent(in) :: A (:,:)
      integer(4), intent(out) :: x1, y1, x2, y2
      real(8), allocatable :: p (:)
      integer(4) :: h, g, i, j, N, M
      real(8) :: maximum, t
      N = size(A, 1); M = size(A, 2)
      allocate(p(M))
      
      maximum = 0; x1  = 0; y1  = 0; x2  = 0; y2  = 0
      do g = 1, N
        do j = 1, M
          p(j) = 0
        end do
        do i = g, N
          t = 0; h = 1
          do j = 1 , M
            p(j) = p(j) + A(i,j); t = t+p(j)
            if (t>maximum) then
              maximum = t; x1 = g; y1 = h; x2 = i; y2 = j
            else if (t<=0) then
              t = 0; h = j+1
            end if
          end do
        end do
      end do
    end subroutine
end module
