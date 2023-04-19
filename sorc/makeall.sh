#!/bin/bash

#modules :
module load PrgEnv-intel/8.3.3
module load netcdf/4.7.4
module load intel/19.1.3.304

echo ifort = `which ifort`
ftn = `which ftn`

make
