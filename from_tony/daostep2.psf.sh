#!/bin/csh
#
# Script made by Sangmo Sohn, 10/20/2000
#
# Shell script for performing PSF photometry with DAOPHOT/ALLSTAR
# This script must be run AFTER the interactive selection of PSF stars.
#
alias rm rm
set image=${1}
#  Delete files to avoid errors in DAOPHOT II.
if (-e ${image}.plst  ) rm ${image}.plst
if (-e ${image}.psf   ) rm ${image}.psf
if (-e ${image}.nei   ) rm ${image}.nei
if (-e ${image}.grp   ) rm ${image}.grp
if (-e ${image}.nst   ) rm ${image}.nst
if (-e ${image}a.imh  ) rm ${image}a.imh
if (-e ${image}a.pix  ) rm ${image}a.pix
if (-e ${image}s.imh  ) rm ${image}s.imh
if (-e ${image}s.pix  ) rm ${image}s.pix
if (-e ${image}.als   ) rm ${image}.als
date >> ${image}.log
# Run DAOPHOT to subtract the neighbor stars.
daophot.psf << DONE >>! ${image}.log
opt
${image}.opt

attach $image
psf
${image}.ap
${image}.nlst
${image}.psf
exit
DONE
if (-e temp1 ) rm temp1
if (-e temp2 ) rm temp2
if (-e temp2 ) rm temp3
head -3 ${image}.nlst > ${image}.plst
tail +4 ${image}.nlst > temp1
tail +4 ${image}.nei >> temp1
sort temp1 > temp2
awk '{print $2,$3,$4,$5,$1}' temp2 > temp3
uniq -d -f4 temp3 | awk '{printf "%6d%9.3f%9.3f%9.3f%9.3f\n",$5,$1,$2,$3,$4}' - >> ${image}.plst
rm temp1
rm temp2
rm temp3
daophot.psf << DONE >>! ${image}.log
opt
${image}.opt

attach $image
group
${image}.nei
${image}.psf
1.
${image}.grp
nstar
${image}.psf
${image}.grp
${image}.nst
substar
${image}.psf
${image}.nst
y
${image}.plst
${image}a

# Now attach the neghbor-subtracted image and get the final PSF.
attach ${image}a
psf
${image}.nst
${image}.plst
${image}.psf


exit
DONE
# Run ALLSTAR to do the actual photometry
cat ${image}.als.opt > allstar.inp
echo $image         >> allstar.inp
echo $image'.psf'   >> allstar.inp
echo $image'.ap'    >> allstar.inp
echo $image'.als'   >> allstar.inp
echo $image's'      >> allstar.inp
allstar < allstar.inp >>! ${image}.log
rm allstar.inp
date >> ${image}.log
# Are you happy now ?
