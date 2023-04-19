OBJ = math.o drifter.o io_nc.o main.o 

include mk.wcoss2

all : $(OBJ)
	$(FC) $(OBJ)  $(LIBDIRS)  -l netcdff -o all

clean :
	        rm *.o *.mod output.nc

distclean : clean
	        rm all

