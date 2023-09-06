#!/bin/bash

#wcoss2 modules :
module load PrgEnv-intel/8.3.3
module load netcdf/4.7.4
module load intel-classic/2022.2.0.262

echo zzz ifort = `which ifort`
ftn = `which ftn`


make
