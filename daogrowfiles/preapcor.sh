#!/bin/sh
#
#############################################################################
#
# PREAPCOR.SH - Script to perform preparations for DAOGROW
#
# USAGE - ./preapcor.sh <image name without extension>
#
# HISTORY:
#
# Adapted from Script made by Sangmo Tony Sohn autopsf.sh
# Edited to only run steps necessary for DAOGROW. 
# These steps are included in autopsf.sh. If additional work is done
# on the PSF, this will need to be run to do DAOGROW.
# The name of the file with PSF LIST is assumed to be .plst
#
# 11/08/2006 Rachael Beaton UVA
#
#############################################################################
#
daophot="/net/halo/bin/daophot"
allstar="/net/halo/bin/allstar"
#goodpsf="/net/halo/bin/goodpsf"
#lstfilter="/net/halo/bin/lstfilter"
export image=${1}
#
#  If required files do not exist, don't run this script.
#
if [ ! -s ${image}.fits ]; then
   echo "ERROR: input image [ ${image}.fits ] not found."
   exit 1
fi
if [ ! -s ${image}.opt ]; then
   echo "ERROR: ${image}.opt required to run DAOPHOT."
   exit 1
fi
if [ ! -s photo.opt ]; then
   echo "ERROR: photo.opt required to run DAOPHOT."
   exit 1
fi
if [ ! -s apcor.opt ]; then
   echo "ERROR: apcor.opt required to run DAOPHOT."
   exit 1
fi
if [ ! -s ${image}.als.opt ]; then
   echo "ERROR: ${image}.opt.als required to run ALLSTAR."
   exit 1
fi
if [ ! -s ${image}.nei ]; then
   echo "ERROR: ${image}.nei required to Prep for Daogrow."
   exit
fi
if [ ! -s ${image}.psf ]; then
echo "ERROR: ${image}.psf required to Prep for Daogrow."
   exit
fi
if [ ! -s ${image}.plst ]; then
echo "ERROR: ${image}.plst required to Prep for Daogrow."
   exit
fi
#
# Enter DAOPHOT and create a.fits for subtracted stars in SUBSTAR
#
cp ${image}.opt daophot.opt
rm ${image}.nst    >& /dev/null
rm ${image}a.fits  >& /dev/null
rm ${image}.grp    >& /dev/null
#
daophot << END_DAOPHOT >> ${image}.log
OPTIONS
${image}.opt

ATTACH $image
GROUP
${image}.nei
${image}.psf
1.0
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

EXIT
END_DAOPHOT
#
##################################################
###                                            ###
### 5th step : get readied for running DAOGROW ###
###                                            ###
##################################################
#
echo "   Running 5th step ...."
rm ${image}a.log   >& /dev/null
rm ${image}a.ap    >& /dev/null
rm ${image}a.als   >& /dev/null
rm ${image}as.fits >& /dev/null
rm allstar.opt     >& /dev/null
#
echo "" >> ${image}.log
echo "===============================================" >> ${image}.log
echo "== Step 5 : Prepare for aperture correction. ==" >> ${image}.log
echo "===============================================" >> ${image}.log
echo "" >> ${image}.log
#
#  Run DAOPHOT on the image with neighbor-subtracted PSF stars.
#
daophot << END_DAOPHOT >> ${image}a.log
OPTIONS
${image}.opt

ATTACH ${image}a
PHOTOMETRY
apcor.opt

${image}.plst
${image}a.ap
exit
END_DAOPHOT
cat ${image}.als.opt > allstar.opt
cat ${image}.als.opt > allstar.inp
echo ${image}a      >> allstar.inp
echo ${image}.psf   >> allstar.inp
echo ${image}a.ap   >> allstar.inp
echo ${image}a.als  >> allstar.inp
echo ${image}as     >> allstar.inp
allstar < allstar.inp >> ${image}a.log
rm allstar.inp
rm ${image}as.fits >& /dev/null
echo ""
#
#
