module kadane_mpi
	implicit none
	contains
		subroutine kadane
			use mpi
!			include "mpif.h"
			integer(4) :: maximum, t, r1, c1, r2, c2, g, N, M, h, i, j
			integer(4), allocatable :: a (:,:)  !integer(4), dimension(:,:), allocatable :: a
			integer(4), allocatable :: p (:)    !integer(4), dimension(:  ), allocatable :: p
			real :: r
			integer(4) :: mpi_master, mpi_rank, mpi_size, mpi_err
			integer(4), dimension(2) :: Ms, Mso
			INTEGER :: status(MPI_STATUS_SIZE)
			call mpi_init(mpi_err)
			mpi_master = 0
			call mpi_comm_rank(mpi_comm_world, mpi_rank, mpi_err)
			call mpi_comm_size(mpi_comm_world, mpi_size, mpi_err)
			if (mpi_rank == mpi_master) then
				write(*,*) "N, M = ?" 
				read(*,*) N, M
			end if
			call mpi_bcast(N, 1, mpi_integer, mpi_master, mpi_comm_world, mpi_err)
			call mpi_barrier(mpi_comm_world, mpi_err)
			call mpi_bcast(M, 1, mpi_integer, mpi_master, mpi_comm_world, mpi_err)
			call mpi_barrier(mpi_comm_world, mpi_err)
			allocate(a(N,M))
			allocate(p(M))
			if (mpi_rank == mpi_master) then
				call random_seed
				do i = 1, N
					do j = 1, M
						call random_number(r)
						a(i,j) = floor(r * 100-50) 
						write (*,"(I5)", advance="no") a(i,j)
					end do
					write (*,*)
				end do
			end if
			call mpi_bcast(a, N*M, mpi_integer, mpi_master, mpi_comm_world, mpi_err)
			call mpi_barrier(mpi_comm_world, mpi_err)

			maximum = 0; r1  = 0; c1  = 0; r2  = 0; c2  = 0
			do g = 1, N
				if (mod(g, mpi_size) == mpi_rank) then
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
				end if
			end do
			Ms(1) = maximum; Ms(2) = mpi_rank
			call mpi_barrier(mpi_comm_world, mpi_err)
			call mpi_allreduce(Ms,Mso,1,mpi_2integer,mpi_maxloc,mpi_comm_world,mpi_err)
			call mpi_barrier(mpi_comm_world, mpi_err)
!		 	write (*,*) "#", mpi_rank, M, r1, c1, r2, c2
			if(Mso(2) /= mpi_master) then
				if(Mso(2) == mpi_rank) then
					call mpi_send(r1, 1, mpi_integer, mpi_master, 0, mpi_comm_world, mpi_err)
					call mpi_send(r2, 1, mpi_integer, mpi_master, 1, mpi_comm_world, mpi_err)
					call mpi_send(c1, 1, mpi_integer, mpi_master, 2, mpi_comm_world, mpi_err)
					call mpi_send(c2, 1, mpi_integer, mpi_master, 3, mpi_comm_world, mpi_err)
				else if(mpi_rank == mpi_master) then
					maximum = Mso(1)
					call mpi_recv(r1, 1, mpi_integer, Mso(2), 0, mpi_comm_world, status, mpi_err)
					call mpi_recv(r2, 1, mpi_integer, Mso(2), 1, mpi_comm_world, status, mpi_err)
					call mpi_recv(c1, 1, mpi_integer, Mso(2), 2, mpi_comm_world, status, mpi_err)
					call mpi_recv(c2, 1, mpi_integer, Mso(2), 3, mpi_comm_world, status, mpi_err)
				end if
			end if
			call mpi_barrier(mpi_comm_world, mpi_err)
			if(mpi_rank == mpi_master) then
				write (*,*) "Sum is:", maximum
				write (*,"(A,I3,A,I3,A,I3,A,I3,A)") "(",r1, ",",c1, ") (",r2, ",",c2,")"
			end if
			call mpi_finalize(mpi_err)
		end subroutine
end module kadane_mpi
