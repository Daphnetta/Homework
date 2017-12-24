module Homework
  contains
    subroutine FindMaxCoordinates(A, x1, y1, x2, y2)
      real(8), intent(in), dimension(:,:) :: A
      integer(4), intent(out) :: x1, y1, x2, y2
      integer :: omp_get_thread_num, omp_get_num_threads
      real(8), allocatable :: p (:)
      integer(4) :: h, g, i, j, N, M, x1_thread, x2_thread, y1_thread, y2_thread
      real(8) :: maximum, temporary_sum, maximum_thread
      N = size(A, 1); M = size(A, 2)

      maximum        = A(1,1); x1         = 1; y1         = 1; x2         = 1; y2         = 1
      maximum_thread = A(1,1); x1_thread  = 1; y1_thread  = 1; x2_thread  = 1; y2_thread  = 1
!$omp parallel shared(A, maximum, x1, x2, y1, y2) &
!$omp& firstprivate(p,g,h,i,j,temporary_sum,maximum_thread,x1_thread,x2_thread,y1_thread,y2_thread)
      allocate(p(M))
!$omp do schedule(dynamic, 1)
      do g = 1, N
        do j = 1, M
          p(j) = 0
        end do
        do i = g, N
          temporary_sum = 0; h = 1
          do j = 1 , M
            p(j) = p(j) + A(i,j); temporary_sum = temporary_sum+p(j)
            if (temporary_sum > maximum_thread) then
              maximum_thread = temporary_sum; x1_thread = g; y1_thread = h; x2_thread = i; y2_thread = j
            else if (temporary_sum <= 0) then
              temporary_sum = 0; h = j + 1
            end if
          end do
        end do
      end do
!$omp end do
!$omp critical

      if(maximum_thread>maximum) then
        maximum = maximum_thread
        x1 = x1_thread; x2 = x2_thread;
        y1 = y1_thread; y2 = y2_thread;
      endif
!$omp end critical
!$omp end parallel

    end subroutine
end module
