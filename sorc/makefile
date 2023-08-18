OBJ = math.o drifter.o io_nc.o main.o read.o run.o write.o 

include mk.hera

all : $(OBJ)
	echo $(NETCDF_LIB)
	$(FC) $(OBJ)  $(NETCDF_LIB)  -l netcdff -o all

clean :
	        rm *.o *.mod output.nc

distclean : clean
	        rm all

