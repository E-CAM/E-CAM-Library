#!/bin/bash

source ~/gmx_abrupt_adress/bin/GMXRC

export GMX_MAXCONSTRWARN=-1

i=$1
j=$2

case $3 in
 [1]*)
#intial run: first density without thermodyn. Force  
rm F_th_* tabletf* *.dens.s* dens_mix_* \#* *.dens_s*.xvg
./TF_calc_water_xplit_sphere.sh 0  dens.REF.out CALC.dens.out s0 1

#thermodyn. Force iterations from scratch (!!!) from $i (Start) to $j (end) 
for z in `seq $i $j`; do
#sphere choice
./TF_calc_water_xplit_sphere.sh  2  dens.REF.out CALC.dens.out s$z 1
# xsplit choice
./TF_calc_water_xplit_sphere.sh  2  dens.REF.out CALC.dens.out s$z 2
done
#The scripts prints out the forces and densities for each iteration.

;;
 [2]*)
#This is the restart from iteration $i to $j from an existing(!!!) thermodyn. Force.
for z in `seq $i $j`; do
#sphere choice
./TF_calc_water_xplit_sphere.sh  2  dens.REF.out CALC.dens.out s$z 1
# xsplit choice
./TF_calc_water_xplit_sphere.sh  2  dens.REF.out CALC.dens.out s$z 2
done

;;
esac


