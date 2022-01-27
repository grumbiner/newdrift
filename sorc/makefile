SRC = math.f90 drifter.f90 io_nc.f90 main.f90 
OBJ = math.o drifter.o io_nc.o main.o 

FOPTS = -c -Wall -std=f2008 -I ~/usrlocal/include
FC = gfortran

all : $(OBJ)
	gfortran $(OBJ)  -L ~/usrlocal/lib -l netcdff -o all

clean :
	rm *.o *.mod all

%.o: %.f90
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F90
	$(FC) $(FOPTS) $< -o $(@)
