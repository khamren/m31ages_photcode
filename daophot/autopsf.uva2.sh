#!/bin/sh
#
#############################################################################
#
# AUTOPSF.SH - Script to perform automated PSF photometry using DAOPHOT II
#
# USAGE - ./autopsf.sh <image name w/o extension>
#
# HISTORY - 01/07/2005 : Created by altering daostep1.sh daostep2.sh.
#
#           02/03/2005 : Modified - run another pass of goodpsf after
#                        subtracting neighbor stars.
#
#           02/03/2005 : Added the goodpsf.f F77 program to the end of 
#                        this script file and commented out.
#
#           02/07/2005 : Added the preapcor.sh script at the end
#
#           03/31/2005 : Modified to use LSTFILTER and added lstfilter.f
#                        F77 program to the end of this script.
#
#           04/19/2005 : Added WHILE loop in 2nd step for initial filtering.
#
#
# Script made by Sangmo Tony Sohn
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
   echo "ERROR: ${image}.opta required to run ALLSTAR."
   exit 1
fi
if [ ! -s goodpsf ]; then
   echo "ERROR: GOODPSF program required to filter bad PSF stars."
   exit 1
fi
if [ ! -s lstfilter ]; then
   echo "ERROR: LSTFILTER program required to filter bad PSF stars."
   exit 1
fi
#
echo "Starting photometry on ${image}.fits :"
#
###############################################################
###                                                         ###
### 1st step : select PSF candidates and obtain initial PSF ###
###                                                         ###
###############################################################
#
echo "   Running 1st step ...."
#
#  Delete files to avoid errors in DAOPHOT.
#
rm ${image}.log      >& /dev/null
rm ${image}.coo      >& /dev/null
rm ${image}.ap       >& /dev/null
rm ${image}.lst      >& /dev/null
rm ${image}.lst1     >& /dev/null
rm ${image}.psf      >& /dev/null
rm ${image}.nei      >& /dev/null
rm ${image}.psf.log  >& /dev/null
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
FIND
1,1
${image}.coo
y
PHOTOMETRY


${image}.coo
${image}.ap
PICKPSF
${image}.ap
100,25
${image}.lst
EXIT
END_DAOPHOT
#
#  Filter bad sources from ${image}.lst and copy it as ${image}.lst1 using LSTFILTER
#
lstfilter << END_LSTFILTER >& /dev/null
${image}.lst
${image}.coo
${image}.lst1
END_LSTFILTER
#
#  Run DAOPHOT to construct a first version of PSF.
#
daophot << END_DAOPHOT >> ${image}.psf.log
OPTIONS
${image}.opt

ATTACH $image
PSF
${image}.ap
${image}.lst1
${image}.psf

EXIT
END_DAOPHOT
#
#################################################################################
###                                                                           ###
### 2nd step : filter out bad PSF stars and construct a second version of PSF ###
###                                                                           ###
#################################################################################
#
echo "   Running 2nd step ...."
#
rm ${image}.lst2     >& /dev/null
rm ${image}.lst1.chi ${image}.lst2.chi >& /dev/null
#
echo "" >> ${image}.log
echo "===================================" >> ${image}.log
echo "== Step 2 : Construct PSF ver. 2 ==" >> ${image}.log
echo "===================================" >> ${image}.log
echo "" >> ${image}.log
#
ln1=`grep -n "Profile errors:" ${image}.psf.log | sed 's/:/ /g' | awk '(NR==1){print $1+2}'`
ln2=`grep -n "File with PSF stars and neighbors =" ${image}.psf.log | sed 's/:/ /g' | awk '(NR==1){print $1-4}'`
nl=$((ln2-ln1+1))
tail -n +${ln1} ${image}.psf.log | head -n ${nl} - | sed -e 's/saturated/ 1.000   /g' -e 's/defective/ 1.000   /g' > ${image}.lst1.chi
#
cp ${image}.lst1 ${image}.lst2
cp ${image}.lst1.chi ${image}.lst2.chi
nbad=100
#
#######
#
#  The WHILE loop for rejecting stars with either '*' or '?' next to them.
#
while [ $nbad -gt 0 ]; do 
goodpsf << END_GOODPSF >& /dev/null
${image}.lst2
${image}.lst2.chi
${image}.lstt
END_GOODPSF
#
rm ${image}.psf.log
rm ${image}.lst2 ${image}.lst2.chi
mv ${image}.lstt ${image}.lst2
#
daophot << END_DAOPHOT >> ${image}.psf.log
OPTIONS
${image}.opt

ATTACH ${image}
PSF
${image}.ap
${image}.lst2
${image}.psf


EXIT
END_DAOPHOT
#
ln1=`grep -n "Profile errors:" ${image}.psf.log | sed 's/:/ /g' | awk '(NR==1){print $1+2}'`
ln2=`grep -n "File with PSF stars and neighbors =" ${image}.psf.log | sed 's/:/ /g' | awk '(NR==1){print $1-4}'`
nl=$((ln2-ln1+1))
tail -n +${ln1} ${image}.psf.log | head -n ${nl} - | sed -e 's/saturated/ 1.000   /g' -e 's/defective/ 1.000   /g' > ${image}.lst2.chi
nbad=`grep -e '?' -e '*' ${image}.lst2.chi | wc -l`
done
#
# End of WHILE loop
#
#######
#
####################################################################
###                                                              ###
### 3rd step : subtract neighbor stars and construct a final PSF ###
###                                                              ###
####################################################################
#
echo "   Running 3rd step ...."
#
rm ${image}.grp      >& /dev/null
rm ${image}.nst      >& /dev/null
rm ${image}a.fits    >& /dev/null
rm ${image}.lst2.chi >& /dev/null
rm ${image}.lst3     >& /dev/null
rm ${image}.lst3.chi >& /dev/null
rm ${image}.plst     >& /dev/null
rm ${image}.psf.log  >& /dev/null
rm ${image}.plst.chi >& /dev/null
rm ${image}.psf.log  >& /dev/null
#
echo "" >> ${image}.log
echo "=============================================" >> ${image}.log
echo "== Step 3 : Construct final version of PSF ==" >> ${image}.log
echo "=============================================" >> ${image}.log
echo "" >> ${image}.log
#
#  Run DAOPHOT to subtract negihbor stars.
#
daophot << END_DAOPHOT >> ${image}.log
OPTIONS
${image}.opt

ATTACH $image
GROUP
${image}.nei
${image}.psf
5.
${image}.grp
NSTAR
${image}.psf
${image}.grp
${image}.nst
SUBSTAR
${image}.psf
${image}.nst
y
${image}.lst2
${image}a

EXIT
END_DAOPHOT
#
#  Run DAOPHOT to create ${image}.psf.log
#
daophot << END_DAOPHOT >> ${image}.psf.log
OPTIONS
${image}.opt

ATTACH ${image}a
PSF
${image}.ap
${image}.lst2
${image}.psf


EXIT
END_DAOPHOT
#
ln1=`grep -n "Profile errors:" ${image}.psf.log | sed 's/:/ /g' | awk '(NR==1){print $1+2}'`
ln2=`grep -n "File with PSF stars and neighbors =" ${image}.psf.log | sed 's/:/ /g' | awk '(NR==1){print $1-4}'`
nl=$((ln2-ln1+1))
tail -n +${ln1} ${image}.psf.log | head -n ${nl} - | sed -e 's/saturated/ 1.000   /g' -e 's/defective/ 1.000   /g' > ${image}.lst2.chi
#
cp ${image}.lst2 ${image}.lst3
cp ${image}.lst2.chi ${image}.lst3.chi
nbad=100
#
#######
#
#  The WHILE loop for rejecting stars with either '*' or '?' next to them.
#
while [ $nbad -gt 0 ]; do 
goodpsf << END_GOODPSF >& /dev/null
${image}.lst3
${image}.lst3.chi
${image}.plst
END_GOODPSF
#
rm ${image}.psf.log
rm ${image}.lst3 ${image}.lst3.chi
mv ${image}.plst ${image}.lst3
#
daophot << END_DAOPHOT >> ${image}.psf.log
OPTIONS
${image}.opt

ATTACH ${image}a
PSF
${image}.ap
${image}.lst3
${image}.psf


EXIT
END_DAOPHOT
#
ln1=`grep -n "Profile errors:" ${image}.psf.log | sed 's/:/ /g' | awk '(NR==1){print $1+2}'`
ln2=`grep -n "File with PSF stars and neighbors =" ${image}.psf.log | sed 's/:/ /g' | awk '(NR==1){print $1-4}'`
nl=$((ln2-ln1+1))
tail -n +${ln1} ${image}.psf.log | head -n ${nl} - | sed -e 's/saturated/ 1.000   /g' -e 's/defective/ 1.000   /g' > ${image}.lst3.chi
nbad=`grep -e '?' -e '*' ${image}.lst3.chi | wc -l`
done
#
# End of WHILE loop
#
#######
#
mv ${image}.lst3 ${image}.plst
mv ${image}.lst3.chi ${image}.plst.chi
#rm ${image}.lst1 ${image}.lst2 ${image}.lst1.chi ${image}.lst2.chi
#
######################################################
###                                                ###
### 4th step : perform PSF photometry with ALLSTAR ###
###                                                ###
######################################################
#
echo "   Running 4th step ...."
#
rm allstar.inp    >& /dev/null
rm ${image}.als   >& /dev/null
rm ${image}s.fits >& /dev/null
#
echo "" >> ${image}.log
echo "===========================" >> ${image}.log
echo "== Step 4 : Run ALLSTAR. ==" >> ${image}.log
echo "===========================" >> ${image}.log
echo "" >> ${image}.log
#
cat ${image}.als.opt > allstar.inp
echo $image         >> allstar.inp
echo $image'.psf'   >> allstar.inp
echo $image'.ap'    >> allstar.inp
echo $image'.als'   >> allstar.inp
echo $image's'      >> allstar.inp
allstar < allstar.inp >> ${image}.log
rm allstar.inp >& /dev/null
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
#
#
######################################################################
#
# Below, I include the FORTRAN77 code -- goodpsf.f, in case I might 
# accidentally delete it.  Take out the preceding "#"s (commented), 
# copy and paste to a file called "goodpsf.f", and compile the 
# FORTRAN source using the following command.
#
# $ g77 goodpsf.f -o goodpsf
#
# Tested under gcc version 3.3.5.
#
######################################################################
#
#      PROGRAM GOODPSF
#C
#C=====================================================================
#C
#C This program generates a file with only good PSF candidates.
#C
#C=====================================================================
#C
#      INTEGER nmax
#      REAL minchi
#      PARAMETER(nmax=1000,minchi=0.5)
#C
#C 'minchi' is the minimum CHI value allowed for a PSF star.
#C You may change this value to fit your needs.
#C
#      INTEGER i,j,k,n,nr,id(nmax),iid(nmax),nstar,npsf,istar,ibadstar
#      REAL x(nmax),y(nmax),mag(nmax),err(nmax),sky(nmax)
#      REAL chi(nmax)
#      CHARACTER flag(nmax)*1,line*85
#      CHARACTER*60 lstfile,chifile,outfile
#      CHARACTER*69 line1,line2
#      
#      PRINT *,'File with PSF candiates (.lst)?'
#      READ(*,'(A60)') lstfile
#      PRINT *,'File with chi values of PSF candidates (.lst.chi)?'
#      READ(*,'(A60)') chifile
#      PRINT *,'Output file ?'
#      READ(*,'(A60)') outfile
#C
#C=====================================================================
#C Count total number of PSF candidates.
#C=====================================================================
#C
#      OPEN(11,FILE=lstfile)
#      READ(11,'(A69)') line1
#      READ(11,'(A69)') line2
#      READ(11,*)
#      nstar = 0
#      DO i = 1, nmax
#         READ(11,*,END=9) id(i),x(i),y(i),mag(i),err(i),sky(i)
#         nstar = nstar + 1
#      ENDDO
#9     CLOSE(11)
#C           
#C=====================================================================
#C Read in chi values and flags from .chi file.
#C Stole this part from Stetson's code (~line 580 of psf.f).
#C=====================================================================
#C
#      k = (nstar-1)/5 + 1
#      OPEN(12,FILE=chifile)
#      DO i = 1, k
#         READ(12,'(A85)') line
#         DO istar = i, nstar, k
#            j = 17*(istar-i)/k + 1
#            READ (line(j:j+16),69) iid(istar),chi(istar),flag(istar)
#         ENDDO
#      ENDDO
#      CLOSE(12)
#69    FORMAT(I7, F7.3, 1X, A1, 1X)
#C           
#C=====================================================================
#C  Process PSF candidates flags and prune out.
#C=====================================================================
#C
#      npsf = nstar
#      OPEN(21,FILE=outfile)
#      WRITE(21,'(A69)') line1
#      WRITE(21,'(A69)') line2
#      WRITE(21,*)
#      DO i = 1, nstar
#         IF (flag(i).NE.'?' .AND. flag(i).NE.'*' 
#     .                      .AND. chi(i).LT.minchi) THEN
#            DO j = 1, nstar
#               IF (id(j).EQ.iid(i)) THEN
#                  WRITE(21,70) id(j),x(j),y(j),mag(j),err(j),sky(j)
#                  GOTO 1
#               ENDIF
#            ENDDO
#         ELSE
#            npsf = npsf -1
#         ENDIF
#1     ENDDO
#      CLOSE(21)
#70    FORMAT(I7,5F9.3)
#C
#      WRITE(6,71) npsf,nstar
#71    FORMAT(/'You have ',I3,' (out of ',I3,') PSF candidates left.'/)
#C
#      STOP
#      END
#
#
#
#
######################################################################
#
# Below, I include the FORTRAN77 code -- lstfilter.f, in case I might 
# accidentally delete it.  Take out the preceding "#"s (commented), 
# copy and paste to a file called "lstfilter.f", and compile the 
# FORTRAN source using the following command.
#
# $ g77 lstfilter.f -o lstfilter
#
# Tested under gcc version 3.3.5.
#
######################################################################
#
#      PROGRAM LSTFILTER
#C
#C=====================================================================
#C
#C This program filters out bad sources from the .lst file
#C
#C=====================================================================
#C
#      INTEGER nmax
#      PARAMETER(nmax=50000)
#C
#      INTEGER i,j,nstar,nnstar,nleft
#      INTEGER id(nmax),cid(nmax)
#      REAL x(nmax),y(nmax),mag(nmax),err(nmax),sky(nmax)
#      REAL sharp(nmax),round(nmax),dum
#      CHARACTER flag(nmax)*1,line*85
#      CHARACTER*60 lstfile,coofile,outfile
#      CHARACTER*69 line1,line2
#      
#      PRINT *,'File with PSF candiates (.lst)?'
#      READ(*,'(A60)') lstfile
#      PRINT *,'File with FIND output (.coo)?'
#      READ(*,'(A60)') coofile
#      PRINT *,'Output file (.lst1) ?'
#      READ(*,'(A60)') outfile
#C
#C=====================================================================
#C Read in .lst file
#C=====================================================================
#C
#      OPEN(11,FILE=lstfile)
#      READ(11,'(A69)') line1
#      READ(11,'(A69)') line2
#      READ(11,*)
#      nstar = 0
#      DO i = 1, nmax
#         READ(11,*,END=9) id(i),x(i),y(i),mag(i),err(i),sky(i)
#         nstar = nstar + 1
#      ENDDO
#9     CLOSE(11)
#C           
#C=====================================================================
#C Read in .coo file
#C=====================================================================
#C
#      OPEN(12,FILE=coofile)
#      READ(12,*)
#      READ(12,*)
#      READ(12,*)
#      nnstar = 0
#      DO i = 1, nmax
#         READ(12,*,END=99) cid(i),dum,dum,dum,sharp(i),round(i),dum
#         nnstar = nnstar + 1
#      ENDDO
#99    CLOSE(12)
#C           
#C=====================================================================
#C Filter out non-stellar sources
#C=====================================================================
#C
#      nleft = nstar
#      OPEN(21,FILE=outfile)
#      WRITE(21,'(A69)') line1
#      WRITE(21,'(A69)') line2
#      WRITE(21,*)
#      DO i = 1, nstar
#         DO j = 1, nnstar
#            IF (id(i).EQ.cid(j)) THEN
#               IF (sharp(j).GT.0.3 .AND. sharp(j).LT.1.0) THEN
#                  WRITE(21,70) id(i),x(i),y(i),mag(i),err(i),sky(i)
#               ELSE
#                  nleft = nleft-1
#               ENDIF
#               GOTO 111
#            ENDIF
#         ENDDO
#111   ENDDO
#70    FORMAT(I7,5F9.3)
#C      
#      WRITE(6,71) nleft,nstar
#71    FORMAT(/'You have ',I3,' (out of ',I3,') stars left.'/)
#C
#      STOP
#      END
