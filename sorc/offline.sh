#!/bin/sh

#initialize
#drift_in -- file with full 6 values drifters, set to -99 for i,j,clat, clon
#  at readin/initialize, set i,j and clat/clon = ilat/ilon
cp drift_ref.nc drift_in.nc

#Loop:
#forecast hours 000 to 072 by 1
#forecast hours 072 to 192 by 3

fname=rtofs_glo_2ds_f${hhh}_ice.nc
echo $fname $dt | ./drifter drift_in.nc drift_out.nc
cp drift_out.nc drift_f${hhh}.nc
mv drift_out.nc drift_in.nc

#endloop

#mv outputs to $com


#For inline:
#  subroutine that accepts initial (full) drifters, forcing, and dt
#    returns updated locations
#  accepts info on whether/where to write out
#offline:
#   
