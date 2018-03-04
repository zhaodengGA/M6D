#-----------------------------------------------------
# Makefile to compile the Ledge6d system.
#-----------------------------------------------------

# Define compilers and flags.
# Compilers and flags

#include ${PETSC_DIR}/conf/variables
# Must include #include "finclude/petsc.h" at 
# top of procedure using PETSC command.


# Compilers
FC     = mpif90  -mkl=sequential

FMATH  = -r8
#FMATH  = -s real64
FOPT   = -O3 -hfp3
FDEBUG = -eD -Ktrap=fp -m 1
FOMP   = -mp=nonuma
FNOMP  =
FHDF5 = -DHAVE_HDF5
#FPETSC = ${FC_LINKER_FLAGS} ${PETSC_FC_INCLUDES} ${PETSC_LIB}

# Archive 
ARCH = ar cr


ifeq ($(OPT),debug)
   FFLAGS=${FDEBUG}
else
   FFLAGS=${FOPT}
endif

EXEC=M6D_driver

LLIB=M6D_lib

OBJECTS = M6D_use_grid.o \
          cdeterminant.o \
          M6D_driver.o \
          M6D_use_dynamic.o \
          M6D_mainsub.o \
          M6D_read_input.o \
          M6D_use_profiles.o \
          M6D_use_matrix.o \
          M6D_use_advancematrix.o \
          M6D_allocate_big.o \
          M6D_setup_grids.o \
          M6D_setup_profiles.o \
          M6D_cmatinverse.o \
          M6D_cmatinverse5.o \
          M6D_setup_advancematrix.o \
          M6D_setup_divgeo.o \
          M6D_initialize.o \
          M6D_explicitadvance.o \
          M6D_getMsupfromM.o \
          M6D_getMfromMsup.o \
          M6D_getEM.o \
          M6D_getBM.o \
          M6D_getCM.o \
          M6D_getdivM.o \
          M6D_getEfield.o \
          M6D_getMclosure.o \
          M6D_getdata.o \
          M6D_deallocate_big.o \
          cmatrix_inverse.o \
          eigenvaluesolver.o \
          M6D_backup.o \
          M6D_restart.o \
          M6D_local_ICmode.o \
          M6D_dispersion_solver.o

.SUFFIXES : .o .f90 .f .F

all: $(LLIB).a $(EXEC)

$(EXEC): $(LLIB).a $(EXEC).o
	$(FC) $(FFLAGS) -o $(EXEC) $(EXEC).o $(LLIB).a $(EXTRA_LIBS)

$(LLIB).a: $(OBJECTS)
	$(ARCH) $(LLIB).a $(OBJECTS)

.f90.o :
	$(FC) $(FFLAGS) $(FMATH) -c $<

.f.o :
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f *.o  $(EXEC) $(LLIB).a
