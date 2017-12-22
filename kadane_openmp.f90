module kadane_openmp
	implicit none
	contains
		subroutine kadane
			integer :: omp_get_thread_num, omp_get_num_threads
			integer(4) :: maximum, t, r1, c1, r2, c2, g, N, M, h, i, j
			integer(4) :: Mo, r1o, r2o, c1o, c2o
			integer(4), allocatable :: a (:,:)  !integer(4), dimension(:,:), allocatable :: a
			integer(4), allocatable :: p (:)    !integer(4), dimension(:  ), allocatable :: p
			real :: r

			write(*,*) "N, M = ?"
			read(*,*) N, M
			allocate(a(N,M))
			call random_seed
			do i = 1, N
				do j = 1, M
					call random_number(r)
					a(i,j) = floor(r * 100-50)
					write (*,"(I5)", advance="no") a(i,j)
				end do
				write (*,*)
			end do
			Mo       = 0; r1o = 0; c1o = 0; r2o = 0; c2o = 0
			maximum  = 0; r1  = 0; c1  = 0; r2  = 0; c2  = 0
!$omp parallel firstprivate(p,g,h,i,j,t,maximum,r1,r2,c1,c2) shared(a, Mo, r1o, r2o, c1o, c2o)
			allocate(p(M))
!$omp do schedule(dynamic, 1)
			do g = 1, N
				do j = 1, M
					p(j) = 0
				end do
				do i = g, N
					t = 0; h = 1
					do j = 1 , M
						p(j) = p(j) + a(i,j); t = t+p(j)
						if (t>maximum) then
							maximum=t; r1=g; c1=h; r2=i; c2=j
						else if (t<=0) then
							t=0; h =j+1
						end if
					end do
				end do
			end do
!$omp end do
!		write (*,*) "#", omp_get_thread_num(), M, r1, c1, r2, c2
!$omp critical
			if(maximum>Mo) then
				Mo = maximum
				c1o = c1; c2o = c2;
				r1o = r1; r2o = r2;
			endif
!$omp end critical
!$omp end parallel
			write (*,*) "Sum is:", Mo
			write (*,"(A,I3,A,I3,A,I3,A,I3,A)") "(",r1o, ",",c1o, ") (",r2o, ",",c2o,")"
		end subroutine
end module kadane_openmp
