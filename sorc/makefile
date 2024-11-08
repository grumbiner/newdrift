OBJ = math.o drifter.o io_nc.o main.o 

include mk.this

drifter : $(OBJ)
	echo zzz netcdf_lib = $(NETCDF_LIBRARIES)
	$(FC) $(OBJ)  -L $(NETCDF_LIBRARIES)  -l netcdff -o $(@)

clean :
	        rm *.o *.mod 

distclean : clean
	        rm drifter

