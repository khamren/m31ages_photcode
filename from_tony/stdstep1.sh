#!/bin/csh
#
# Script made by Sangmo Sohn, 03/13/2001
# Modified : 12/13/2001
#
#  FIRST STAGE : ATTACH -> FIND -> PHOTOMETRY -> PICKSF
#
alias rm rm
set image=${1}
#  Delete files to avoid errors in DAOPHOT II.
if (-e ${image}.log   ) rm ${image}.log
if (-e ${image}.coo   ) rm ${image}.coo
if (-e ${image}.ap    ) rm ${image}.ap
if (-e ${image}.lst   ) rm ${image}.lst
if (-e ${image}.nlst  ) rm ${image}.nlst
date > ${image}.log
# Run DAOPHOT to perfrom FIND and PHOTOMETRY on frame.
daophotf << DONE >>! ${image}.log
opt
${image}.opt

attach $image
find
1,1
${image}.coo
y
photometry


${image}.coo
${image}.ap
pickpsf
${image}.ap
100,20
${image}.lst
DONE
