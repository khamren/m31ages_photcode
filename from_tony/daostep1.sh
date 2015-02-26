#!/bin/csh
#
########################################################################
#
# DAOSTEP1.SH -- Shell script to perform the first few steps of DAOPHOT
#
# USAGE - daostep1.sh <image>
#
# DESCRIPTION - DAOSTEP1.SH goes through the steps ;
#               ATTACH -> FIND -> PHOTOMETRY -> PICKSF
#               Once the .lst files are made, the PSF task must be 
#               run interactively (MON) to chose suitable stars.
#
# Script made by Sangmo Tony Sohn
#
# HISTORY - 09/21/2000 : First version
#           12/13/2001 : Modified script to used DAOPHOTF
#           12/18/2001 : Improvements and added comments
#
########################################################################
alias rm rm
alias cp cp
alias daophotf /home/grads/ss5fb/daophotf/daophotf
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
#  Delete files to avoid errors in DAOPHOT.
#
if (-e ${image}.log   ) rm ${image}.log
if (-e ${image}.coo   ) rm ${image}.coo
if (-e ${image}.ap    ) rm ${image}.ap
if (-e ${image}.lst   ) rm ${image}.lst
#
#  Copy the {image}.opt file to daophot.opt for avoiding error.
#
if (-e daophot.opt    ) rm daophot.opt
cp ${image}.opt daophot.opt
#
#  Run DAOPHOT to perform FIND, PHOTOMETRY, and PICKPSF on frame.
#  Change the number of stars to be picked as appropriate.
#
daophotf << DONE >>! ${image}.log
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
70,25
${image}.lst
DONE
