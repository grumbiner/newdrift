OBJ = math.o drifter.o io_nc.o main.o read.o run.o write.o 

include mk.this_sys

all : $(OBJ)
	$(FC) $(OBJ)  $(LIBDIRS)  -l netcdff -o all

clean :
	        rm *.o *.mod output.nc

distclean : clean
	        rm all

