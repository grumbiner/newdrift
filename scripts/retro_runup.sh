#!/bin/bash 
#PBS -N driftup
#PBS -o driftup
#PBS -j oe
#PBS -A ICE-DEV
#PBS -q dev
#PBS -l walltime=2:00:00
#PBS -l select=1:ncpus=1

set -x

cd $HOME/rgdev/devdrift/scripts

export PDY=20250601
export COMOUT=$HOME/noscrub/devdrift_retro
if [ ! -d $COMOUT ] ; then
  mkdir -p $COMOUT
fi

export end=`date +"%Y%m%d"`
export end=20250601
export end=$PDY

while [ $PDY -le $end ]
do
  if [ ! -d $COMOUT/$PDY ] ; then
    time ./retro.sh > ${PDY}.out
  else
    echo zzz have $PDY already
  fi

  PDY=`expr $PDY + 1`
  PDY=`$HOME/bin/dtgfix3 $PDY`
done
