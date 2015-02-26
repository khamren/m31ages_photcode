\rm wcomp.dat
\rm weights
##
# This Script assumes you have 6 images (1 M, 2 T2, 3 D51).
# The script will go into the .opt & .log file for each image
# and will take out the necessary information for to calculate
# image weights using "getweights".
#
# To use this script you must list the image name in order of 
# the images in the .tfr file from Daomaster.
# example: getweights.sh obj1001 obj1002 obj1003 ....
#
# THe output files will have one line for each of the three
# parameters for the input file (t): Readnoise,
# fwhm and median sky.
#
# Run the comanion script rmweights.sh to remove the uncessesary 
# text.
#
# kmg -- 06/14/2007
##
#export image1=${1}
#export image2=${2}
#export image3=${3}
#export image4=${4}
#export image4=${5}
#export image4=${6}
#export image4=${7}
#export image4=${8}
#set image1=${obj125}
#set image2=${obj126}
#set image3=${obj127}
#set image4=${obj128}
#set image5=${obj129}
#set image6=${obj130}
set image1=${1}
set image2=${2}
set image3=${3}
set image4=${4}
set image5=${5}
set image6=${6}
#set image7=${7}
##
# Get Information from Image1
# Create input files for each chip
##
grep RE ${image1}_1.opt | awk '{print $3;}' > t1
grep FW ${image1}_1.opt | awk '{print $3;}' > t2
grep Clipped ${image1}_1.log | awk '{print $7;}' > t3
paste t1 t2 t3 > w_1.inp
rm -f t1 t2 t3
grep RE ${image1}_2.opt | awk '{print $3;}' > t1
grep FW ${image1}_2.opt | awk '{print $3;}' > t2
grep Clipped ${image1}_2.log | awk '{print $7;}' > t3
paste t1 t2 t3 > w_2.inp
rm -f t1 t2 t3
grep RE ${image1}_3.opt | awk '{print $3;}' > t1
grep FW ${image1}_3.opt | awk '{print $3;}' > t2
grep Clipped ${image1}_3.log | awk '{print $7;}' > t3
paste t1 t2 t3 > w_3.inp
rm -f t1 t2 t3
grep RE ${image1}_4.opt | awk '{print $3;}' > t1
grep FW ${image1}_4.opt | awk '{print $3;}' > t2
grep Clipped ${image1}_4.log | awk '{print $7;}' > t3
paste t1 t2 t3 > w_4.inp
rm -f t1 t2 t3
grep RE ${image1}_5.opt | awk '{print $3;}' > t1
grep FW ${image1}_5.opt | awk '{print $3;}' > t2
grep Clipped ${image1}_5.log | awk '{print $7;}' > t3
paste t1 t2 t3 > w_5.inp
rm -f t1 t2 t3
grep RE ${image1}_6.opt | awk '{print $3;}' > t1
grep FW ${image1}_6.opt | awk '{print $3;}' > t2
grep Clipped ${image1}_6.log | awk '{print $7;}' > t3
paste t1 t2 t3 > w_6.inp
rm -f t1 t2 t3
grep RE ${image1}_7.opt | awk '{print $3;}' > t1
grep FW ${image1}_7.opt | awk '{print $3;}' > t2
grep Clipped ${image1}_7.log | awk '{print $7;}' > t3
paste t1 t2 t3 > w_7.inp
rm -f t1 t2 t3
grep RE ${image1}_8.opt | awk '{print $3;}' > t1
grep FW ${image1}_8.opt | awk '{print $3;}' > t2
grep Clipped ${image1}_8.log | awk '{print $7;}' > t3
paste t1 t2 t3 > w_8.inp
rm -f t1 t2 t3
##
# Get Information from Image2
##
grep RE ${image2}_1.opt | awk '{print $3;}' > t1
grep FW ${image2}_1.opt | awk '{print $3;}' > t2
grep Clipped ${image2}_1.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_1.inp
rm -f t1 t2 t3
grep RE ${image2}_2.opt | awk '{print $3;}' > t1
grep FW ${image2}_2.opt | awk '{print $3;}' > t2
grep Clipped ${image2}_2.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_2.inp
rm -f t1 t2 t3
grep RE ${image2}_3.opt | awk '{print $3;}' > t1
grep FW ${image2}_3.opt | awk '{print $3;}' > t2
grep Clipped ${image2}_3.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_3.inp
rm -f t1 t2 t3
grep RE ${image2}_4.opt | awk '{print $3;}' > t1
grep FW ${image2}_4.opt | awk '{print $3;}' > t2
grep Clipped ${image2}_4.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_4.inp
rm -f t1 t2 t3
grep RE ${image2}_5.opt | awk '{print $3;}' > t1
grep FW ${image2}_5.opt | awk '{print $3;}' > t2
grep Clipped ${image2}_5.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_5.inp
rm -f t1 t2 t3
grep RE ${image2}_6.opt | awk '{print $3;}' > t1
grep FW ${image2}_6.opt | awk '{print $3;}' > t2
grep Clipped ${image2}_6.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_6.inp
rm -f t1 t2 t3
grep RE ${image2}_7.opt | awk '{print $3;}' > t1
grep FW ${image2}_7.opt | awk '{print $3;}' > t2
grep Clipped ${image2}_7.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_7.inp
rm -f t1 t2 t3
grep RE ${image2}_8.opt | awk '{print $3;}' > t1
grep FW ${image2}_8.opt | awk '{print $3;}' > t2
grep Clipped ${image2}_8.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_8.inp
rm -f t1 t2 t3
##
# Get Information from Image3
##
grep RE ${image3}_1.opt | awk '{print $3;}' > t1
grep FW ${image3}_1.opt | awk '{print $3;}' > t2
grep Clipped ${image3}_1.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_1.inp
rm -f t1 t2 t3
grep RE ${image3}_2.opt | awk '{print $3;}' > t1
grep FW ${image3}_2.opt | awk '{print $3;}' > t2
grep Clipped ${image3}_2.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_2.inp
rm -f t1 t2 t3
grep RE ${image3}_3.opt | awk '{print $3;}' > t1
grep FW ${image3}_3.opt | awk '{print $3;}' > t2
grep Clipped ${image3}_3.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_3.inp
rm -f t1 t2 t3
grep RE ${image3}_4.opt | awk '{print $3;}' > t1
grep FW ${image3}_4.opt | awk '{print $3;}' > t2
grep Clipped ${image3}_4.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_4.inp
rm -f t1 t2 t3
grep RE ${image3}_5.opt | awk '{print $3;}' > t1
grep FW ${image3}_5.opt | awk '{print $3;}' > t2
grep Clipped ${image3}_5.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_5.inp
rm -f t1 t2 t3
grep RE ${image3}_6.opt | awk '{print $3;}' > t1
grep FW ${image3}_6.opt | awk '{print $3;}' > t2
grep Clipped ${image3}_6.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_6.inp
rm -f t1 t2 t3
grep RE ${image3}_7.opt | awk '{print $3;}' > t1
grep FW ${image3}_7.opt | awk '{print $3;}' > t2
grep Clipped ${image3}_7.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_7.inp
rm -f t1 t2 t3
grep RE ${image3}_8.opt | awk '{print $3;}' > t1
grep FW ${image3}_8.opt | awk '{print $3;}' > t2
grep Clipped ${image3}_8.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_8.inp
rm -f t1 t2 t3
##
# Get Information from Image4
##
grep RE ${image4}_1.opt | awk '{print $3;}' > t1
grep FW ${image4}_1.opt | awk '{print $3;}' > t2
grep Clipped ${image4}_1.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_1.inp
rm -f t1 t2 t3
grep RE ${image4}_2.opt | awk '{print $3;}' > t1
grep FW ${image4}_2.opt | awk '{print $3;}' > t2
grep Clipped ${image4}_2.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_2.inp
rm -f t1 t2 t3
grep RE ${image4}_3.opt | awk '{print $3;}' > t1
grep FW ${image4}_3.opt | awk '{print $3;}' > t2
grep Clipped ${image4}_3.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_3.inp
rm -f t1 t2 t3
grep RE ${image4}_4.opt | awk '{print $3;}' > t1
grep FW ${image4}_4.opt | awk '{print $3;}' > t2
grep Clipped ${image4}_4.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_4.inp
rm -f t1 t2 t3
grep RE ${image4}_5.opt | awk '{print $3;}' > t1
grep FW ${image4}_5.opt | awk '{print $3;}' > t2
grep Clipped ${image4}_5.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_5.inp
rm -f t1 t2 t3
grep RE ${image4}_6.opt | awk '{print $3;}' > t1
grep FW ${image4}_6.opt | awk '{print $3;}' > t2
grep Clipped ${image4}_6.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_6.inp
rm -f t1 t2 t3
grep RE ${image4}_7.opt | awk '{print $3;}' > t1
grep FW ${image4}_7.opt | awk '{print $3;}' > t2
grep Clipped ${image4}_7.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_7.inp
rm -f t1 t2 t3
grep RE ${image4}_8.opt | awk '{print $3;}' > t1
grep FW ${image4}_8.opt | awk '{print $3;}' > t2
grep Clipped ${image4}_8.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_8.inp
rm -f t1 t2 t3
##
# Get Information from image5
##
grep RE ${image5}_1.opt | awk '{print $3;}' > t1
grep FW ${image5}_1.opt | awk '{print $3;}' > t2
grep Clipped ${image5}_1.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_1.inp
rm -f t1 t2 t3
grep RE ${image5}_2.opt | awk '{print $3;}' > t1
grep FW ${image5}_2.opt | awk '{print $3;}' > t2
grep Clipped ${image5}_2.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_2.inp
rm -f t1 t2 t3
grep RE ${image5}_3.opt | awk '{print $3;}' > t1
grep FW ${image5}_3.opt | awk '{print $3;}' > t2
grep Clipped ${image5}_3.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_3.inp
rm -f t1 t2 t3
grep RE ${image5}_4.opt | awk '{print $3;}' > t1
grep FW ${image5}_4.opt | awk '{print $3;}' > t2
grep Clipped ${image5}_4.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_4.inp
rm -f t1 t2 t3
grep RE ${image5}_5.opt | awk '{print $3;}' > t1
grep FW ${image5}_5.opt | awk '{print $3;}' > t2
grep Clipped ${image5}_5.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_5.inp
rm -f t1 t2 t3
grep RE ${image5}_6.opt | awk '{print $3;}' > t1
grep FW ${image5}_6.opt | awk '{print $3;}' > t2
grep Clipped ${image5}_6.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_6.inp
rm -f t1 t2 t3
grep RE ${image5}_7.opt | awk '{print $3;}' > t1
grep FW ${image5}_7.opt | awk '{print $3;}' > t2
grep Clipped ${image5}_7.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_7.inp
rm -f t1 t2 t3
grep RE ${image5}_8.opt | awk '{print $3;}' > t1
grep FW ${image5}_8.opt | awk '{print $3;}' > t2
grep Clipped ${image5}_8.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_8.inp
rm -f t1 t2 t3
##
# Get Information from image6
##
grep RE ${image6}_1.opt | awk '{print $3;}' > t1
grep FW ${image6}_1.opt | awk '{print $3;}' > t2
grep Clipped ${image6}_1.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_1.inp
rm -f t1 t2 t3
grep RE ${image6}_2.opt | awk '{print $3;}' > t1
grep FW ${image6}_2.opt | awk '{print $3;}' > t2
grep Clipped ${image6}_2.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_2.inp
rm -f t1 t2 t3
grep RE ${image6}_3.opt | awk '{print $3;}' > t1
grep FW ${image6}_3.opt | awk '{print $3;}' > t2
grep Clipped ${image6}_3.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_3.inp
rm -f t1 t2 t3
grep RE ${image6}_4.opt | awk '{print $3;}' > t1
grep FW ${image6}_4.opt | awk '{print $3;}' > t2
grep Clipped ${image6}_4.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_4.inp
rm -f t1 t2 t3
grep RE ${image6}_5.opt | awk '{print $3;}' > t1
grep FW ${image6}_5.opt | awk '{print $3;}' > t2
grep Clipped ${image6}_5.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_5.inp
rm -f t1 t2 t3
grep RE ${image6}_6.opt | awk '{print $3;}' > t1
grep FW ${image6}_6.opt | awk '{print $3;}' > t2
grep Clipped ${image6}_6.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_6.inp
rm -f t1 t2 t3
grep RE ${image6}_7.opt | awk '{print $3;}' > t1
grep FW ${image6}_7.opt | awk '{print $3;}' > t2
grep Clipped ${image6}_7.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_7.inp
rm -f t1 t2 t3
grep RE ${image6}_8.opt | awk '{print $3;}' > t1
grep FW ${image6}_8.opt | awk '{print $3;}' > t2
grep Clipped ${image6}_8.log | awk '{print $7;}' > t3
paste t1 t2 t3 >> w_8.inp
rm -f t1 t2 t3
##
# Get Information from image7
##
#grep RE ${image7}_1.opt | awk '{print $3;}' > t1
#grep FW ${image7}_1.opt | awk '{print $3;}' > t2
#grep Clipped ${image7}_1.log | awk '{print $7;}' > t3
#paste t1 t2 t3 >> w_1.inp
#rm -f t1 t2 t3
#grep RE ${image7}_2.opt | awk '{print $3;}' > t1
#grep FW ${image7}_2.opt | awk '{print $3;}' > t2
#grep Clipped ${image7}_2.log | awk '{print $7;}' > t3
#paste t1 t2 t3 >> w_2.inp
#rm -f t1 t2 t3
#grep RE ${image7}_3.opt | awk '{print $3;}' > t1
#grep FW ${image7}_3.opt | awk '{print $3;}' > t2
#grep Clipped ${image7}_3.log | awk '{print $7;}' > t3
#paste t1 t2 t3 >> w_3.inp
#rm -f t1 t2 t3
#grep RE ${image7}_4.opt | awk '{print $3;}' > t1
#grep FW ${image7}_4.opt | awk '{print $3;}' > t2
#grep Clipped ${image7}_4.log | awk '{print $7;}' > t3
#paste t1 t2 t3 >> w_4.inp
#rm -f t1 t2 t3
#grep RE ${image7}_5.opt | awk '{print $3;}' > t1
#grep FW ${image7}_5.opt | awk '{print $3;}' > t2
#grep Clipped ${image7}_5.log | awk '{print $7;}' > t3
#paste t1 t2 t3 >> w_5.inp
#rm -f t1 t2 t3
#grep RE ${image7}_6.opt | awk '{print $3;}' > t1
#grep FW ${image7}_6.opt | awk '{print $3;}' > t2
#grep Clipped ${image7}_6.log | awk '{print $7;}' > t3
#paste t1 t2 t3 >> w_6.inp
#rm -f t1 t2 t3
#grep RE ${image7}_7.opt | awk '{print $3;}' > t1
#grep FW ${image7}_7.opt | awk '{print $3;}' > t2
#grep Clipped ${image7}_7.log | awk '{print $7;}' > t3
#paste t1 t2 t3 >> w_7.inp
#rm -f t1 t2 t3
#grep RE ${image7}_8.opt | awk '{print $3;}' > t1
#grep FW ${image7}_8.opt | awk '{print $3;}' > t2
#grep Clipped ${image7}_8.log | awk '{print $7;}' > t3
#paste t1 t2 t3 >> w_8.inp
rm -f t1 t2 t3
##
# Paste all the w_?.inp together; form a file with FWHM, avg sky over all 8
# chips
##
paste w_1.inp w_2.inp w_3.inp w_4.inp w_5.inp w_6.inp w_7.inp w_8.inp > wall.tmp
awk '{print $2, ($3+$6+$9+$12+$15+$18+$21+$24)/8.0;}' wall.tmp > wcomp.dat
rm -f w_1.inp w_2.inp w_3.inp w_4.inp w_5.inp w_6.inp w_7.inp w_8.inp
# look at wall.tmp, make sure all 8 chips of an image have same FWHM

# view wall.tmp

# add a first column to wcomp.dat, which is weighting based on expected 
# count level of an M31 star in that image: this can be fancy (use photometric
# calibration, figure out how many counts a star of a certain magnitude 
# w/ a color of a typical M31 RGB star would have in each frame), or can
# assume that typical exposure times are calculated to get the stars of 
# interest, ie. M31 RGB stars to approximately the same depth.  Thus,
# since typically observe 1 900s M, 2 900s T, and 3 1800s DDO, 
# you might choose to make
# the weights in your first column 1.0 if it is an M frame, 2.0 if it is a T
# frame, and 6.0 if it is a DDO frame (ie. a typical M31 RGB star is expected
# to be 1/6 as faint in DDO as in M). When the weights are calculated, this
# column will be in the denominator.  If the exposure times are not nominal,
# you should adjust the expected fractional counts accordingly. 
# Determine weighting for each frame (to be used on all chips). weight 
# by S/N^2, taking into account the seeing of each frame.  multiply by 
# an arbitrary factor so get more human readable numbers


vi wcomp.dat

awk '{print (1.0/$1/$2/$2/$3)*10**4;}' wcomp.dat > weights
rm -f wall.tmp
