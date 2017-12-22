export PATH := "/opt/intel/bin:/opt/intel/compilers_and_libraries_2018.0.128/linux/mpi/intel64/bin:$(PATH)"

COMPILER=ifort
COMPILER_MPI=mpiifort

KAD=kadane
SEQ=$(KAD)_sequential
OMP=$(KAD)_openmp
MPI=$(KAD)_mpi
EXP=f90

.SILENT:
all: seq omp mpi

seq: $(SEQ).$(EXP) main_$(SEQ).$(EXP)
	$(COMPILER)          -o "$(SEQ)" "$(SEQ).$(EXP)" "main_$(SEQ).$(EXP)"

omp: $(OMP).$(EXP) main_$(OMP).$(EXP)
	$(COMPILER) -fopenmp -o "$(OMP)" "$(OMP).$(EXP)" "main_$(OMP).$(EXP)"

mpi: $(MPI).$(EXP) main_$(MPI).$(EXP)
	$(COMPILER_MPI)      -o "$(MPI)" "$(MPI).$(EXP)" "main_$(MPI).$(EXP)"

.PHONY: run
run: run_seq run_omp run_mpi

.PHONY: run_seq
run_seq: seq
	./"$(SEQ)"

.PHONY: run_omp
run_omp: omp
	./"$(OMP)"

.PHONY: run_mpi
run_mpi: mpi
	mpirun ./"$(MPI)"

.PHONY: clean
clean:
	rm -f "$(SEQ)" "$(SEQ).mod"
	rm -f "$(OMP)" "$(OMP).mod"
	rm -f "$(MPI)" "$(MPI).mod"
