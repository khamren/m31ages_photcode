#
## An effective shell script which creates a file with only PSF stars
## This is done by matching the same stars in .lst and .nei files.
#
if (-f temp1 ) \rm temp1
if (-f temp2 ) \rm temp2
if (-f temp2 ) \rm temp3
head -3 ${image}.lst >! ${image}.iter
tail +4 ${image}.lst > temp1
tail +4 ${image}.nei >> temp1
sort temp1 > temp2
awk '{print $2,$3,$4,$5,$1}' temp2 > temp3
uniq -d -f4 temp3 | awk '{printf "%6d%9.3f%9.3f%9.3f%9.3f\n",$5,$1,$2,$3,$4}' - >> ${image}.iter
\rm temp1
\rm temp2
\rm temp3
