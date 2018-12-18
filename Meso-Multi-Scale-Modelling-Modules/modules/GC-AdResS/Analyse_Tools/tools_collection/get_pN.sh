vmd -dispdev text -e extract_coord.tcl
grep -B1 "Frame" WCG.xyz > a
sed '/Frame/ {$!N;d;}' a > column2
grep -B0 "Frame" WCG.xyz > a
sed -i s/Frame// a
sed -i s/--// a
sed -i s/:// a
sed '/^$/d' a > column1
paste column1 column2|awk '{print $1, $2}' > dat.3nm.pn.WCG.dat
