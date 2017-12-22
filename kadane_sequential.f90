module kadane_sequential
	implicit none
	contains
		subroutine kadane
			integer(4) ::maximum, t, r1, c1, r2, c2, g, N, M, h, i, j
			integer(4), allocatable :: a (:,:)  !integer(4), dimension(:,:), allocatable :: a
			integer(4), allocatable :: p (:)    !integer(4), dimension(:  ), allocatable :: p
			real :: r
			
			write(*,*) "N, M = ?" 
			read(*,*) N, M
			allocate(a(N,M))
			allocate(p(M))
			call random_seed
			do i = 1, N
				do j = 1, M
					call random_number(r)
					a(i,j) = floor(r * 100-50) 
					write (*,"(I5)", advance="no") a(i,j)
				end do
				write (*,*)
			end do
			maximum = 0; r1  = 0; c1  = 0; r2  = 0; c2  = 0
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
			write (*,*) "Sum is:", maximum
			write (*,"(A,I3,A,I3,A,I3,A,I3,A)") "(",r1, ",",c1, ") (",r2, ",",c2,")"
		end subroutine
end module kadane_sequential
