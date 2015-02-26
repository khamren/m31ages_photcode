#!/bin/sh
#
#############################################################################
#
# GETPSF.SH - Script to obtain sky background estimations.
#
# USAGE - ./getsky.sh <image name w/o extension>
#
#
#############################################################################
#
daophot="/net/halo/bin/daophot"
allstar="/net/halo/bin/allstar"
#
#
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
#
echo "Starting photometry on ${image}.fits :"
#
###############################################################
###                                                         ###
### Calcuate Sky Background 				    ###
###                                                         ###
###############################################################
#
echo "   Running 1st step ...."
#
#  Delete files to avoid errors in DAOPHOT.
#
rm ${image}.log      >& /dev/null
#
echo "===================================" >> ${image}.log
echo "== Step 1 : Construct PSF ver. 1 ==" >> ${image}.log
echo "===================================" >> ${image}.log
echo "" >> ${image}.log
#
#  Copy the {image}.opt file to daophot.opt for safety.
#
rm daophot.opt >& /dev/null
cp ${image}.opt daophot.opt
#
#  Run DAOPHOT to perform FIND, PHOTOMETRY, and PICKPSF on frame.
#  You may change the number of stars in PICKPSF.
#
daophot << END_DAOPHOT >> ${image}.log
OPTIONS
${image}.opt

ATTACH $image
SKY

EXIT 
END_DAOPHOT

