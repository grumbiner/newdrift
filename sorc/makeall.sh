#!/bin/bash

#modules :

## wcoss2:
#module load PrgEnv-intel/8.3.3
#module load netcdf/4.7.4
#module load intel-classic/2022.2.0.262
#ln -sf mk.wcoss2 mk.this

## gaea:
#module load intel-classic/2022.2.1
#module load cray-hdf5/1.12.2.3 
#module load cray-netcdf/4.9.0.3
#export NETCDF=$NETCDF_DIR
#ln -sf mk.gaea mk.this

## hera:
#module load hpc/1.2.0  hpc-intel/2022.1.2
#module load netcdf/4.7.0
#ln -sf mk.hera mk.this

## desk
ln -sf mk.desk mk.this

echo zzz NETCDF 
echo `env | grep  NETCDF`

make
