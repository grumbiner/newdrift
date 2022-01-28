OBJ = math.o drifter.o io_nc.o main.o 

FOPTS = -c -Wall -std=f2008 -I ~/usrlocal/include
FC = gfortran

all : $(OBJ)
	$(FC) $(OBJ)  -L ~/usrlocal/lib -l netcdff -o all

clean :
	rm *.o *.mod all output.nc

%.o: %.f90
	$(FC) $(FOPTS) $< -o $(@)
%.o: %.F90
	$(FC) $(FOPTS) $< -o $(@)
