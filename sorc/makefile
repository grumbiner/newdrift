#SRC = drifter.f90 hello.f90 io_nc.f90 main.f90 math.f90
#OBJ = drifter.o hello.o io_nc.o main.o math.o
SRC = math.f90 drifter.f90 io_nc.f90 main.f90 
OBJ = math.o drifter.o io_nc.o main.o 

FOPTS = -Wall -std=f2008

all : $(SRC)
	#gfortran -c -Wall $(SRC) $(FOPTS) -I ~/usrlocal/include 
	#gfortran $(OBJ) -L ~/usrlocal/lib -l netcdff -o all
	gfortran $(SRC) $(FOPTS) -I ~/usrlocal/include  -L ~/usrlocal/lib -l netcdff -o all

clean :
	rm *.o *.mod all
