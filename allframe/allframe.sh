#!/bin/sh
##
# ALLFRAME.SH
#
##
#allframe2="/net/halo/bin/allframe.2004.fixed"
export image=${1}
##
# Check for Required Files:
##
if [ ! -s ${image}.mch ]; then
   echo "ERROR: Transformations file not found. [ ${image}.mch ]"
   exit 1
fi
if [ ! -s allframe.opt ]; then
   echo "ERROR: Allframe Input file not found. [ allframe.opt ]"
   exit 1
fi
if [ ! -s ${image}.mag ]; then
   echo "ERROR: Master Star List not found. [ ${image}.mag ]"
   exit 1
fi
#
echo "Starting allframe on ${image}.fits .. "
#
echo "Starting allframe ..." >> ${image}.allframe.log
echo " " >> ${image}.allframe.log
#
echo " " >> allframe2.inp
echo $image'.mch' >> allframe2.inp
echo $image'.mag' >> allframe2.inp
echo " " >> allframe2.inp
allframe.2004.fixed < allframe2.inp >> ${image}.allframe.log
rm allframe2.inp >& /dev/null
echo "Allframe Complete."
#
