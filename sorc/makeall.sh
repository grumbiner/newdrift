#!/bin/bash

#modules :
#module load PrgEnv-intel/8.3.3
module load intel-classic/2022.2.0.262
module load netcdf/4.7.4

echo zzz ifort = `which ifort`

make
