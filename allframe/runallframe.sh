##
# Copy files for running allframe to each directory.
##
echo "Copying allframe.sh to chip directories."
cp allframe.sh ./chip1/
cp allframe.sh ./chip2/
cp allframe.sh ./chip3/
cp allframe.sh ./chip4/
cp allframe.sh ./chip5/
cp allframe.sh ./chip6/
cp allframe.sh ./chip7/
cp allframe.sh ./chip8/
echo "Copying allframe.opt to chip directories."
cp allframe.opt ./chip1/
cp allframe.opt ./chip2/
cp allframe.opt ./chip3/
cp allframe.opt ./chip4/
cp allframe.opt ./chip5/
cp allframe.opt ./chip6/
cp allframe.opt ./chip7/
cp allframe.opt ./chip8/
echo "Copy Complete."
##
# Chip 1
##
echo "Starting Chip 1"
cd ./chip1/
# Remove old files
\rm allf.mch
\rm allf.allframe.log
\rm allf.tfr
\rm obj*.alf
\rm obj*j.fits
\rm allf.nmg
echo "Old Allframe files deleted."
cp chip1.mch allf.mch
sed s/.als/.alf/g chip1.mch > chip1.allf.mch
echo "chip1.mch file copied to allf.mch ."
echo "chip1.allf.mch created." 
allframe.sh allf
echo "Chip 1 Finished, details in allf.allframe.log ."
##
# Chip 2
## 
echo "Starting Chip 2"
cd ../chip2/
\rm allf.mch
\rm allf.allframe.log
\rm allf.tfr
\rm obj*.alf
\rm obj*j.fits
\rm allf.nmg
echo "Old Allframe files deleted."
cp chip2.mch allf.mch
sed s/.als/.alf/g chip2.mch > chip2.allf.mch
echo "chip2.mch file copied to allf.mch ."
echo "chip2.allf.mch created."
allframe.sh allf
echo "Chip 2 Finished, details in allf.allframe.log ."
##
# Chip 3
##
echo "Starting Chip 3"
cd ../chip3/
\rm allf.mch
\rm allf.allframe.log
\rm allf.tfr
\rm obj*.alf
\rm obj*j.fits
\rm allf.nmg
echo "Old Allframe files deleted."
cp chip3.mch allf.mch
sed s/.als/.alf/g chip3.mch > chip3.allf.mch
echo "chip3.mch file copied to allf.mch ."
echo "chip3.allf.mch created."
allframe.sh allf
echo "Chip 3 Finished, details in allf.allframe.log ."
##
# Chip 4
## 
echo "Starting Chip 4"
cd ../chip4/
\rm allf.mch
\rm allf.allframe.log
\rm allf.tfr
\rm obj*.alf
\rm obj*j.fits
\rm allf.nmg
echo "Old Allframe files deleted."
cp chip4.mch allf.mch
sed s/.als/.alf/g chip4.mch > chip4.allf.mch
echo "chip4.mch file copied to allf.mch ."
echo "chip4.allf.mch created."
allframe.sh allf
echo "Chip 4 Finished, details in allf.allframe.log ."
##
# Chip 5
##
echo "Starting Chip 5"
cd ../chip5/
\rm allf.mch
\rm allf.allframe.log
\rm allf.tfr
\rm obj*.alf
\rm obj*j.fits
\rm allf.nmg
echo "Old Allframe files deleted."
cp chip5.mch allf.mch
sed s/.als/.alf/g chip5.mch > chip5.allf.mch
echo "chip5.mch file copied to allf.mch ."
echo "chip5.allf.mch created."
allframe.sh allf
echo "Chip 5 Finished, details in allf.allframe.log ."
##
# Chip 6
##
echo "Starting Chip 6"
cd ../chip6/
\rm allf.mch
\rm allf.allframe.log
\rm allf.tfr
\rm obj*.alf
\rm obj*j.fits
\rm allf.nmg
echo "Old Allframe files deleted."
cp chip6.mch allf.mch
sed s/.als/.alf/g chip6.mch > chip6.allf.mch
echo "chip6.mch file copied to allf.mch ."
echo "chip6.allf.mch created."
allframe.sh allf
echo "Chip 6 Finished, details in allf.allframe.log ."
##
# Chip 7
##
echo "Starting Chip 7"
cd ../chip7/
\rm allf.mch
\rm allf.allframe.log
\rm allf.tfr
\rm obj*.alf
\rm obj*j.fits
\rm allf.nmg
echo "Old Allframe files deleted."
cp chip7.mch allf.mch
sed s/.als/.alf/g chip7.mch > chip7.allf.mch
echo "chip7.mch file copied to allf.mch ."
echo "chip7.allf.mch created."
allframe.sh allf
echo "Chip 7 Finished, details in allf.allframe.log ."
## 
# Chip 8
##
echo "Starting Chip 8"
cd ../chip8/
\rm allf.mch
\rm allf.allframe.log
\rm allf.tfr
\rm obj*.alf
\rm obj*j.fits
\rm allf.nmg
echo "Old Allframe files deleted."
cp chip8.mch allf.mch
sed s/.als/.alf/g chip8.mch > chip8.allf.mch
echo "chip8.mch file copied to allf.mch ."
echo "chip8.allf.mch created."
allframe.sh allf
cd ../
echo "Allframe Complete on all chips."
echo "Ready for Daomaster."
