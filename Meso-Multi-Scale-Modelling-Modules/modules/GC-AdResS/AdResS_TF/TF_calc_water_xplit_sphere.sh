#!/bin/bash

source /<our path here>/gromacs/bin/GMXRC

j=$1
l=$2
k=$3
m=$4
n=$5

rmin= "Start of interpolation zone"
rmax= "End of interpolation zone"
rbox= "total length of the box"
rref= "Reference coordinate used in the AdResS input" 
lc= "how fine the grid for the density should be"
prefac= "either select the prefactor from the formula or define a value here"

rstep=$(echo "$rbox/($lc-1)" | bc -l)

r_cut=$rmin
rm_c=$rmax 
echo $r_cut, $rm_c

prep_sys=$j
case $prep_sys in
 [0]*)

if [ ! -f F_th_step0 ]; then 
   seq -f%g 0.000 $rstep $rbox | sed 's/,/./' | awk '{print $1, "0.0", "0.0"}'> F_th_step0
fi
cp F_th_step0 tabletf_WCG.xvg
gmx mdrun -v

;;
 [1]*)

if [ -f tabletf_CAT.xvg ]; then 
   awk 'BEGIN{OFS="\t"}(NR>9){print $1, $2, $3}' tabletf_CAT.xvg > F_th_step0
fi
if [ -f tabletf_ANN.xvg ]; then 
   awk 'BEGIN{OFS="\t"}(NR>9){print $1, $2, $3}' tabletf_ANN.xvg > F_th_step0
fi

;;
 [2]*)

if [ -f tabletf_WCG.xvg ]; then 
   awk 'BEGIN{OFS="\t"}(NR>9){print $1, $2, $3}' tabletf_WCG.xvg > F_th_step0
fi
;;
esac

#rn=$(tail -n 1 F_th_step0 | awk '{printf("%f\n",$1)}')
rn1=$(echo $rn+$rstep | bc -l)
echo $rn $rn1

dens_sys=$n
dens_chk=$n

case $dens_sys in
 [0]*)

  csg_density --axis r --rmax 9.6  --ref [5.5,5.5,5.5] --trj ../7000_mmin/traj_comp.xtc --top ../7000_mmin/topol.tpr  --out test.dens.comp
  csg_density --axis r --rmax 9.6  --ref [5.5,5.5,5.5] --cg "MIL.cg.xml;cl.cg.xml" --trj traj_comp.xtc --top topol.tpr --molname Cl --out Cl.dens.out

;;
 [1]*)
 source ~/votca_5.1/bin/VOTCARC.bash 
 csg_density --axis r --rmax 10.  --ref [4.89439,4.89439,4.89439] --trj traj_comp.xtc --top topol.tpr  --out SOL.dens.dat

 sed 's/nan/0.0/' SOL.dens.dat > calc_dens

  seq -f%g 0 $rstep $rbox | sed 's/,/./' | awk 'BEGIN{OFS="\t"}{print $1, "575.0"}' > ref_dens

;;
 [2]*)

  echo "2" > t_c; gmx density -d X -sl $lc -f traj_comp.xtc -o SOL.dens.xvg < t_c
  awk 'BEGIN{OFS="\t"}(NR>24){print $1, $2}' SOL.dens.xvg > $k
  seq -f%g 0 $rstep $rbox | sed 's/,/./' | awk 'BEGIN{OFS="\t"}{print $1, "1000.0"}' > $l

#folding and symmetrizing density for smoothing:
# give absolute values back: z=${res/#-/}; echo $z
awk '{d=$1-'$rref'; print ((d>0)?d:-d), $2}' $l > ref_dens_t
awk '{d=$1-'$rref'; print ((d>0)?d:-d), $2}' $k >  calc_dens_t

rm x1 x2 xaa xab ref_dens calc_dens
#lc1=$(wc -l ref_dens_t | awk '{print $1/2}')
#echo $lc1
split -l $(( ($lc-1)/2 )) ref_dens_t
awk '{printf "%10f %10f\n", $1, $2}' xaa | sort -g > x1
awk '{printf "%10f %10f\n", $1, $2}' xab | sort -g > x2
paste x1 x2 | awk '{print $1,($2+$4)/2.0}' > ref_dens
#awk '{print $1,$2}' x2 > ref_dens

split -l $(( ($lc-1)/2 )) calc_dens_t
awk '{printf "%10f %10f\n", $1, $2}' xaa | sort -g > x1
awk '{printf "%10f %10f\n", $1, $2}' xab | sort -g > x2
paste x1 x2 | awk '{print $1,($2+$4)/2.0}' > calc_dens

;;
esac


seq -f%g 0.0 $rstep $rbox | sed 's/,/./' > xnew_ntrpl

paste calc_dens ref_dens | awk '{if($1<('$r_cut')) print $1,($2-$4)}' > dens_mix_at
paste calc_dens ref_dens | awk '{if(($1>=('$r_cut')) && ($1<('$rm_c'))) print $1,$2,$4}' > dens_mix_hy
paste calc_dens ref_dens | awk '{if($1>=('$rm_c')) print $1,($2-$4)}' > dens_mix_cg

./smooth_dens.sh $prefac $rstep 400.

cp dens_smooth d_s
cp pot_smooth p_s

echo "#","manually gen. Thermodyn. Force approx." > $m.xvg
echo "#","Parameter:">> $m.xvg
echo "#","start hy-region:", $rmin >> $m.xvg
echo "#","end of hy-region:",$rmax >> $m.xvg
echo "#","start tf: from xsplit" >> $m.xvg
echo "#","start hy-region:", $rmin >> $m.xvg
echo "#","end of hy-region:",$rmax >> $m.xvg
echo "#" >> $m.xvg
echo "#" >> $m.xvg

fmax=$(head -n 1 d_s | awk '{print $2}')
fmin=$(tail -n 1 d_s | awk '{print $2}')

pmax=$(head -n 1 p_s | awk '{print $2}')
pmin=$(tail -n 1 p_s | awk '{print $2}')

r_hy_1=$(head -n 1 d_s | awk '{print $1-'$rstep'}')
r_hy_2=$(tail -n 1 d_s | awk '{print $1}')

rbox1=$(echo $rbox-$rstep | bc -l)

seq -f%g 0.000 $rstep $r_hy_1 | sed 's/,/./' | awk '{print $1, 0.0}' > d0
awk '{print $1, $2}' d_s > d1
seq -f%g $r_hy_2 $rstep $rbox1 | sed 's/,/./' | awk '{print $1, 0.0}' > d2

seq -f%g 0.000 $rstep $r_hy_1 | sed 's/,/./' | awk '{print $1, '$pmax'}' > p0
awk '{print $1, $2}' p_s > p1
seq -f%g $r_hy_2 $rstep $rbox1 | sed 's/,/./' | awk '{print $1, '$pmin'}' > p2

cat d0 d1 d2 > d_m
cat p0 p1 p2 > p_m
paste p_m d_m | awk '{print $1,$2,$4}' > dens_mix
paste F_th_step0 dens_mix | awk 'BEGIN{OFS="\t"}{printf("%e %e %e\n",$1,($2-$5),($3-$6)) }' >> $m.xvg

cp $m.xvg tabletf_WCG_$m.xvg
cp $m.xvg tabletf_WCG.xvg

#clean-up:
gmx mdrun -v

dens_chk=$n
case $dens_chk in
 [0]*)

  csg_density --axis r --rmax 9.6  --ref [5.5,5.5,5.5] --trj ../7000_mmin/traj_comp.xtc --top ../7000_mmin/topol.tpr  --out test.dens.comp

;;
 [1]*)

 source ~/votca_5.1/bin/VOTCARC.bash 
 csg_density --axis r --rmax 10.  --ref [4.89439,4.89439,4.89439] --trj traj_comp.xtc --top topol.tpr  --out SOL.dens.xvg
 sed 's/nan/0.0/' SOL.dens.xvg > SOL.dens_$m.xvg

;;
 [2]*)

   echo "2" > t_c; gmx density -d X -f traj_comp.xtc -o SOL.dens_$m.xvg < t_c

;;
esac


# CLEAN UP
rm p? d? x? x?? \#*
rm SOL.dpot.*
rm p_m d_m
rm d_s d_s_t 
rm t_c
rm s?.xvg s??.xvg
rm ref_dens
rm ref_dens_t calc_dens*
rm s.d.o 
rm p_s
rm ref_t 
rm calc_t 
rm dens.ref
