#
# Prep Files for Astrometry.
#
# Make Lists for Wcsctran
#
ls obj*_?.fits > fits.list
ls chip?.mch > mch.list
sed s/.mch/.xy/g mch.list > xy.list
\rm mch.list
sed s/.xy/.radec/g xy.list > radec.list
#
# Make .xy files from .stuffprob files.
# 
awk '{print $1, $2, $3;}' chip1.stuffprob > chip1.xy
awk '{print $1, $2, $3;}' chip2.stuffprob > chip2.xy
awk '{print $1, $2, $3;}' chip3.stuffprob > chip3.xy
awk '{print $1, $2, $3;}' chip4.stuffprob > chip4.xy
awk '{print $1, $2, $3;}' chip5.stuffprob > chip5.xy
awk '{print $1, $2, $3;}' chip6.stuffprob > chip6.xy
awk '{print $1, $2, $3;}' chip7.stuffprob > chip7.xy
awk '{print $1, $2, $3;}' chip8.stuffprob > chip8.xy
