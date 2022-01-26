#SRC = drifter.f90 hello.f90 io_nc.f90 main.f90 math.f90
#OBJ = drifter.o hello.o io_nc.o main.o math.o
SRC = drifter.f90 io_nc.f90 main.f90 
OBJ = drifter.o io_nc.o main.o 

FOPTS = -frecursive

all : $(SRC)
	#gfortran -c -Wall $(SRC) $(FOPTS) -I ~/usrlocal/include 
	#gfortran $(OBJ) -L ~/usrlocal/lib -l netcdff -o all
	gfortran -Wall  $(SRC) $(FOPTS) -I ~/usrlocal/include  -L ~/usrlocal/lib -l netcdff -o all

clean :
	rm *.o *.mod all
