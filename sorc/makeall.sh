#!/bin/bash

#modules :
# wcoss2:
module load PrgEnv-intel/8.3.3
module load netcdf/4.7.4
module load intel-classic/2022.2.0.262

# gaea:
#module load intel-classic/2022.2.1
#module load cray-hdf5/1.12.2.3 
#module load cray-netcdf/4.9.0.3
#export NETCDF=$NETCDF_DIR

# hera:
#module load hpc/1.2.0  hpc-intel/2022.1.2
#module load netcdf/4.7.0


echo zzz ifort = `which ifort`
echo zzz NETCDF 
env | grep  NETCDF

make
