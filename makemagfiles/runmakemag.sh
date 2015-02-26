##
# Run makemag in all allfdirectories.
##
cp /net/halo/rlb9n/scripts/makemagfiles/makemag .
echo 'Copying code ...'
cp ./makemag ./chip1
cp ./makemag ./chip2
cp ./makemag ./chip3
cp ./makemag ./chip4
cp ./makemag ./chip5
cp ./makemag ./chip6
cp ./makemag ./chip7
cp ./makemag ./chip8
echo 'Starting to run makemag ..'
# Chip 1
cd ./chip1/
\rm makemag.log chip.makemag
makemag << END_MAKEMAG > makemag.log
allf.tfr
6,10000000
chip1.makemag
2
END_MAKEMAG
# Chip 2
cd ../chip2/
\rm makemag.log chip.makemag
makemag << END_MAKEMAG > makemag.log
allf.tfr
6,20000000
chip2.makemag
2
END_MAKEMAG
# Chip 3
cd ../chip3/
\rm makemag.log chip.makemag
makemag << END_MAKEMAG > makemag.log
allf.tfr
6,30000000
chip3.makemag
2
END_MAKEMAG
# Chip 4
cd ../chip4/
\rm makemag.log chip.makemag
makemag << END_MAKEMAG > makemag.log
allf.tfr
6,40000000
chip4.makemag
2
END_MAKEMAG
# Chip 5
cd ../chip5/
\rm makemag.log chip.makemag
makemag << END_MAKEMAG > makemag.log
allf.tfr
6,50000000
chip5.makemag
2
END_MAKEMAG
# Chip 6
cd ../chip6/
\rm makemag.log chip.makemag
makemag << END_MAKEMAG > makemag.log
allf.tfr
6,60000000
chip6.makemag
2
END_MAKEMAG
# Chip 7
cd ../chip7/
\rm makemag.log chip.makemag
makemag << END_MAKEMAG > makemag.log
allf.tfr
6,70000000
chip7.makemag
2
END_MAKEMAG
# Chip 8
cd ../chip8/
\rm makemag.log chip.makemag
makemag << END_MAKEMAG > makemag.log
allf.tfr
6,80000000
chip8.makemag
2
END_MAKEMAG
cd ../
