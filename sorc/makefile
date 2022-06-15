OBJ = math.o drifter.o io_nc.o main.o 

include mk.wcoss

all : $(OBJ)
	$(FC) $(OBJ)  -L $(NETCDF_LIB) -l netcdff -o all
