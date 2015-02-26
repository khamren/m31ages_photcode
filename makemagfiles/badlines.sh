#cp /net/halo/rlb9n/scripts/getandremoveoverlap.txt ./
#mv getandremoveoverlap.txt getoverlap.txt
\rm chip?.overlap
python getoverlap.txt chip1.makemag > chip1.overlap
python getoverlap.txt chip2.makemag > chip2.overlap
python getoverlap.txt chip3.makemag > chip3.overlap
python getoverlap.txt chip4.makemag > chip4.overlap
python getoverlap.txt chip5.makemag > chip5.overlap
python getoverlap.txt chip6.makemag > chip6.overlap
python getoverlap.txt chip7.makemag > chip7.overlap
python getoverlap.txt chip8.makemag > chip8.overlap
