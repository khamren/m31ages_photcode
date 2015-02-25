#/bin/csh
#
# Script made by Sangmo Sohn, 03/13/2001
# Modified : 12/13/2001
#
#  SECOND STAGE : Concentric photometry (prepare for DAOGROW)
#
alias rm rm
set image=${1}
#  Delete files to avoid errors in DAOPHOT II.
if (-e ${image}a.ap   ) rm ${image}a.ap
date >> ${image}.log
# Run selstdstar to make *a.coo files.
selstdstar << DONE >>! ${image}.log
${image}.ap
${image}.coord
${image}.lst
${image}a.coo
DONE
# Run DAOPHOT to do concentric aperture photometry.
# The output file (*a.ap) is used in DAOGROW.
daophot << DONE >>! ${image}.log
opt
${image}.opt

attach $image
photometry
apcor.opt

${image}a.coo
${image}a.ap

DONE
