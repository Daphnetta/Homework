program main_kadane_mpi
  use Homework_MPI
  use mpi
  real(8), allocatable :: A (:,:)
  integer(4) :: x1, y1, x2, y2, i ,j, N, M
  real(8) :: r
  integer(4) :: mpi_master, mpi_rank, mpi_err

  call mpi_init(mpi_err)
  mpi_master = 0
  call mpi_comm_rank(mpi_comm_world, mpi_rank, mpi_err)

  if (mpi_rank == mpi_master) then
     write(*,*) "N, M = ?" 
     read(*,*) N, M
  end if
  call mpi_bcast(N, 1, mpi_integer, mpi_master, mpi_comm_world, mpi_err)
  call mpi_bcast(M, 1, mpi_integer, mpi_master, mpi_comm_world, mpi_err)
  allocate(A(N,M))
  if (mpi_rank == mpi_master) then
     call random_seed
     do i = 1, N
        do j = 1, M
           call random_number(r)
           A(i,j) = r * 100-50 
           write (*,"(F5.1,A)", advance="no") A(i,j), " "
        end do
        write (*,*)
     end do
  end if
  call mpi_bcast(A, N*M, mpi_real8, mpi_master, mpi_comm_world, mpi_err)

  call FindMaxCoordinates(A, x1, y1, x2, y2)
  if(mpi_rank == mpi_master) then
     write (*,"(A,I3,A,I3,A,I3,A,I3,A)") "(",x1, ",",y1, ") (",x2, ",",y2,")"
  end if
  call mpi_finalize(mpi_err)
end program
