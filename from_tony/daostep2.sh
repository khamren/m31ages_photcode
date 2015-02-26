#!/bin/csh
#
##########################################################################
#
# DAOSTEP2.SH -- Shell script to construct PSF and perform PSF photometry
#
# USAGE - daostep2.sh <image>
#
# DESCRIPTION - DAOSTEP2.SH goes through the steps requiered to construct
#               and improve the PSF for a given image and runs ALLSTAR 
#               to perform the PSF photometry.
#
# Script made by Sangmo Tony Sohn
#
# HISTORY - 10/20/2000 : First version
#           12/13/2001 : Modified script to used DAOPHOTF
#           12/18/2001 : Improvements and added comments
#
##########################################################################
alias rm rm
alias cp cp
alias daophotf /home/grads/ss5fb/daophotf/daophotf
alias allstarf /home/grads/ss5fb/daophotf/allstarf
set image=${1}
#
#  If ${image}.opt does not exist, don't run this script.
#
if (-e ${image}.opt   ) then
   echo GOOD : ${image}.opt found.
else
   echo ERROR : ${image}.opt required to run DAOPHOT !
   exit
endif
#
#  If ${image}.als.opt does not exist, don't run this script.
#
if (-e ${image}.als.opt ) then
   echo GOOD : ${image}.als.opt found.
else
   echo ERROR : ${image}.als.opt required to run ALLSTAR !
   exit
endif
#
#  Delete files to avoid errors in DAOPHOT and ALLSTAR.
#
if (-e ${image}.plst  ) rm ${image}.plst
if (-e ${image}.grp   ) rm ${image}.grp
if (-e ${image}.nst   ) rm ${image}.nst
if (-e ${image}a.fits ) rm ${image}a.fits
if (-e ${image}s.fits ) rm ${image}s.fits
if (-e ${image}.als   ) rm ${image}.als
#
#  Make .plst file using AWK.  
#  This file only contains the PSF stars selected interactively.
#
if (-e temp1 ) rm temp1
if (-e temp2 ) rm temp2
if (-e temp2 ) rm temp3
head -3 ${image}.lst > ${image}.plst
tail +4 ${image}.lst > temp1
tail +4 ${image}.nei >> temp1
sort temp1 > temp2
awk '{print $2,$3,$4,$5,$1}' temp2 > temp3
uniq -d -f4 temp3 | awk '{printf "%6d%9.3f%9.3f%9.3f%9.3f\n",$5,$1,$2,$3,$4}' - >> ${image}.plst
rm temp1
rm temp2
rm temp3
#
#  Run DAOPHOT to subtract the neighbor stars and
#  construct the final version of PSF.
#
daophotf << DONE >>! ${image}.log
OPTIONS
${image}.opt

ATTACH $image
GROUP
${image}.nei
${image}.psf
1.
${image}.grp
NSTAR
${image}.psf
${image}.grp
${image}.nst
SUBSTAR
${image}.psf
${image}.nst
y
${image}.plst
${image}a

ATTACH ${image}a
PSF
${image}.nst
${image}.plst
${image}.psf


exit
DONE
#
#  Run ALLSTAR.
#
cat ${image}.als.opt > allstar.inp
echo $image         >> allstar.inp
echo $image'.psf'   >> allstar.inp
echo $image'.ap'    >> allstar.inp
echo $image'.als'   >> allstar.inp
echo $image's'      >> allstar.inp
allstarf < allstar.inp >>! ${image}.log
rm allstar.inp
