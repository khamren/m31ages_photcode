#!/bin/csh
#
alias rm rm
alias cp cp
alias daophot.psf /home/grads/ss5fb/daophot/daophot.psf
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
#  If ${image}.als.opt does not exist, don't run this script.
#
if (-e ${image}.als.opt ) then
   echo GOOD : ${image}.als.opt found.
else
   echo ERROR : ${image}.als.opt required to run ALLSTAR !
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
#  Delete files to avoid errors in DAOPHOT.
#
if (-e ${image}.log   ) rm ${image}.log
if (-e ${image}.coo   ) rm ${image}.coo
if (-e ${image}.ap    ) rm ${image}.ap
if (-e ${image}.lst   ) rm ${image}.lst
if (-e ${image}.psf   ) rm ${image}.psf
if (-e ${image}.nei   ) rm ${image}.nei
if (-e ${image}.iter  ) rm ${image}.iter
if (-e ${image}.grp   ) rm ${image}.grp
if (-e ${image}.nst   ) rm ${image}.nst
if (-e ${image}a.fits ) rm ${image}a.fits
if (-e ${image}s.fits ) rm ${image}s.fits
if (-e ${image}.als   ) rm ${image}.als
#
#  Copy the {image}.opt file to daophot.opt for avoiding error.
#
if (-e daophot.opt    ) rm daophot.opt
cp ${image}.opt daophot.opt
#
#  Run DAOPHOT to perform FIND, PHOTOMETRY, and PICKPSF on frame.
#  Change the number of stars to be picked as appropriate.
#
daophot.psf << DONE >>! ${image}.log
OPTIONS
${image}.opt

ATTACH $image
FIND
1,1
${image}.coo
y
PHOTOMETRY


${image}.coo
${image}.ap
PICKPSF
${image}.ap
50,25
${image}.lst
PSF
${image}.ap
${image}.lst
${image}.psf

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
${image}.iter
${image}a

ATTACH ${image}a
PSF
${image}.nst
${image}.iter
${image}.psf


exit
DONE
#
#  Run ALLSTAR.
#
if (-e allstar.opt    ) rm allstar.opt
cat ${image}.als.opt > allstar.opt
cat ${image}.als.opt > allstar.inp
echo $image         >> allstar.inp
echo $image'.psf'   >> allstar.inp
echo $image'.ap'    >> allstar.inp
echo $image'.als'   >> allstar.inp
echo $image's'      >> allstar.inp
allstar < allstar.inp >>! ${image}.log
rm allstar.inp
