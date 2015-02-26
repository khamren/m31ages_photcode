#!/bin/csh
#
alias rm rm
alias daophot /home/grads/ss5fb/daophot/daophot
alias allstar /home/grads/ss5fb/daophot/allstar
set image=${1}
#
#  If ${image}.opt does not exist, don't run DAOPHOT.
#
if (-e ${image}.opt   ) then
   echo GOOD : ${image}.opt found.
else
   echo ERROR : ${image}.opt required to run DAOPHOT !
   exit
endif
#
#  If photo.opt does not exist, don't run DAOPHOT.
#
if (-e photo.opt      ) then
   echo GOOD : photo.opt found.
   echo GOOD : Starting DAOPHOT
else
   echo ERROR : photo.opt required to run DAOPHOT !
   exit
endif
#
#  Delete files to avoid errors in DAOPHOT and ALLSTAR.
#
if (-e ${image}a.log   ) rm ${image}a.log
if (-e ${image}a.ap    ) rm ${image}a.ap
if (-e ${image}a.als   ) rm ${image}a.als
if (-e ${image}as.fits ) rm ${image}as.fits
if (-e allstar.inp     ) rm allstar.inp
#
#  Run DAOPHOT on the image with neighbor-subtracted PSF stars.
#
daophot << DONE >>! ${image}a.log
OPTIONS
${image}.opt

ATTACH ${image}a
PHOTOMETRY


${image}.iter
${image}a.ap
exit
DONE
if (-e allstar.opt ) rm allstar.opt
cat ${image}.als.opt > allstar.opt
cat ${image}.als.opt > allstar.inp
echo ${image}a      >> allstar.inp
echo ${image}.psf   >> allstar.inp
echo ${image}a.ap   >> allstar.inp
echo ${image}a.als  >> allstar.inp
echo ${image}as     >> allstar.inp
allstar < allstar.inp >>! ${image}a.log
rm allstar.inp
rm ${image}as.imh
rm ${image}as.pix
