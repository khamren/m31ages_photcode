echo "runallframe.sh Starting"
echo "This will get a psf for each chip,"
echo "create chip? directory, move files,"
echo "copy files in chip driectory."
##
# Chip 1
##
getpsf.sh allf_1
echo "Made Directory for Chip1"
echo "Moving Files ..."
mkdir chip1
mv chip1.* ./chip1/
mv *_1* ./chip1/
cp ./chip1/allf_1.fits ./chip1/allf.fits
cp ./chip1/allf_1.als.opt ./chip1/allstar.opt
cp ./chip1/allf_1.opt ./chip1/daophot.opt
cp ./chip1/allf_1.als ./chip1/allf.als
echo "File Copy Complete"
##
# Chip 2
##
echo "Starting Daophot for Chip2"
getpsf.sh allf_2
mkdir chip2
echo "Made Directory for Chip2"
echo "Moving Files ..."
mv chip2.* ./chip2/
mv *_2* ./chip2/
cp ./chip2/allf_2.fits ./chip2/allf.fits
cp ./chip2/allf_2.als.opt ./chip2/allstar.opt
cp ./chip2/allf_2.opt ./chip2/daophot.opt
cp ./chip2/allf_2.als ./chip2/allf.als
echo "File Copy Complete"
##
# Chip 3
##
echo "Starting Daophot for Chip3"
getpsf.sh allf_3
mkdir chip3
echo "Made Directory Chip3"
echo "Moving Files ..."
mv chip3.* ./chip3/
mv *_3* ./chip3/
cp ./chip3/allf_3.fits ./chip3/allf.fits
cp ./chip3/allf_3.als.opt ./chip3/allstar.opt
cp ./chip3/allf_3.opt ./chip3/daophot.opt
cp ./chip3/allf_3.als ./chip3/allf.als
echo "File Copy Complete"
##
# Chip 4
##
echo "Starting Daophot for Chip4"
getpsf.sh allf_4
mkdir chip4
echo "Made Directory Chip4"
echo "Moving Files ..."
mv chip4.* ./chip4/
mv *_4* ./chip4/
cp ./chip4/allf_4.fits ./chip4/allf.fits
cp ./chip4/allf_4.als.opt ./chip4/allstar.opt
cp ./chip4/allf_4.opt ./chip4/daophot.opt
cp ./chip4/allf_4.als ./chip4/allf.als
echo "File Copy Complete"
##
# Chip 5
##
echo "Starting Chip5 Daophot"
getpsf.sh allf_5
mkdir chip5
echo "Made Directory Chip5"
echo "Starting Chip5 Daophot"
mv chip5.* ./chip5/
mv *_5* ./chip5/
cp ./chip5/allf_5.fits ./chip5/allf.fits
cp ./chip5/allf_5.als.opt ./chip5/allstar.opt
cp ./chip5/allf_5.opt ./chip5/daophot.opt
cp ./chip5/allf_5.als ./chip5/allf.als
echo "File Copy Complete"
##
# Chip 6
##
echo "Starting Chip6 Daophot"
getpsf.sh allf_6
mkdir chip6
echo "Made Directory Chip6"
echo "Moving Files ..."
mv chip1.* ./chip6/
mv *_6* ./chip6/
echo "File Move Complete"
echo "Starting File Copy"
cp ./chip6/allf_6.fits ./chip6/allf.fits
cp ./chip6/allf_6.als.opt ./chip6/allstar.opt
cp ./chip6/allf_6.opt ./chip6/daophot.opt
cp ./chip6/allf_6.als ./chip6/allf.als
cp ./chip6/allf_6.psf ./chip6/allf.psf
echo "File Copy Complete"
##
# Chip 7
##
echo "Starting Chip7 Daophot"
getpsf.sh allf_7
mkdir chip7
echo "Made Directory Chip7"
echo "Moving Files ..."
mv chip7.* ./chip7/
mv *_7* ./chip7/
echo "File Move Complete"
echo "Starting File Copy"
cp ./chip7/allf_7.fits ./chip7/allf.fits
cp ./chip7/allf_7.als.opt ./chip7/allstar.opt
cp ./chip7/allf_7.opt ./chip7/daophot.opt
cp ./chip7/allf_7.als ./chip7/allf.als
cp ./chip7/allf_7.psf ./chip7/allf.psf
echo "File Copy Complete"
##
# Chip 8
##
echo "Starting Chip8 Daophot"
getpsf.sh allf_8
mkdir chip8
echo "Made Directory Chip8"
echo "Moving Files ..."
mv chip8.* ./chip8/
mv *_8* ./chip8/
echo "File Move Complete"
echo "Starting File Copy"
cp ./chip8/allf_8.fits ./chip8/allf.fits
cp ./chip8/allf_8.als.opt ./chip8/allstar.opt
cp ./chip8/allf_8.opt ./chip8/daophot.opt
cp ./chip8/allf_8.als ./chip8/allf.als
cp ./chip8/allf_8.psf ./chip8/allf.psf
echo "File Copy Complete"
echo "Get PSF Complete for All Chips!"
echo "Success!!"
echo "Copying SExtractor default files into directories."
cp ./default.* ./chip1
cp ./default.* ./chip2
cp ./default.* ./chip3
cp ./default.* ./chip4
cp ./default.* ./chip5
cp ./default.* ./chip6
cp ./default.* ./chip7
cp ./default.* ./chip8
echo "Copy Complete."
echo "Its time for allfprep."
