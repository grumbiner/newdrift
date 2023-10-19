OBJ = math.o drifter.o io_nc.o main.o 

include mk.this

all : $(OBJ)
	echo zzz netcdf_lib = $(NETCDF_LIBRARIES)
	$(FC) $(OBJ)  -L $(NETCDF_LIBRARIES)  -l netcdff -o all

clean :
	        rm *.o *.mod output.nc

distclean : clean
	        rm all

