export PATH := /opt/intel/bin:/opt/intel/compilers_and_libraries_2018.0.128/linux/mpi/intel64/bin:$(PATH)

COMPILER=ifort
COMPILER_MPI=mpiifort

KAD=kadane
SEQ=$(KAD)_sequential
OMP=$(KAD)_openmp
MPI=$(KAD)_mpi
EXP=f90

.SILENT:
all: seq omp mpi

seq: $(SEQ)/$(SEQ).$(EXP) $(SEQ)/main_$(SEQ).$(EXP)
	cd $(SEQ); $(COMPILER)          -o "$(SEQ)" "$(SEQ).$(EXP)" "main_$(SEQ).$(EXP)"

omp: $(OMP)/$(OMP).$(EXP) $(OMP)/main_$(OMP).$(EXP)
	cd $(OMP); $(COMPILER) -fopenmp -o "$(OMP)" "$(OMP).$(EXP)" "main_$(OMP).$(EXP)"

mpi: $(MPI)/$(MPI).$(EXP) $(MPI)/main_$(MPI).$(EXP)
	cd $(MPI); $(COMPILER_MPI)      -o "$(MPI)" "$(MPI).$(EXP)" "main_$(MPI).$(EXP)"

.PHONY: run
run: run_seq run_omp run_mpi

.PHONY: run_seq
run_seq: seq
	$(SEQ)/$(SEQ)

.PHONY: run_omp
run_omp: omp
	$(OMP)/$(OMP)

.PHONY: run_mpi
run_mpi: mpi
	mpirun $(MPI)/$(MPI)

.PHONY: clean
clean:
	rm -f $(SEQ)/$(SEQ)
	rm -f $(OMP)/$(OMP)
	rm -f $(MPI)/$(MPI)
	rm -f */*.mod
