##
# Script to run stuffprob on all chips
##
cp /net/halo/rlb9n/scripts/stuffprobfiles/stuffprob  ./
\rm stuffprob_?.log
# Chip 1
stuffprob << END_STUFFPROB > stuffprob_1.log
chip1.magma
sex_1.repl
1000000
chip1.stuffprob
END_STUFFPROB
# Chip 2
stuffprob << END_STUFFPROB > stuffprob_2.log
chip2.magma
sex_2.repl
2000000
chip2.stuffprob
END_STUFFPROB
# Chip 3
stuffprob << END_STUFFPROB > stuffprob_3.log
chip3.magma
sex_3.repl
3000000
chip3.stuffprob
END_STUFFPROB
# Chip 4
stuffprob << END_STUFFPROB > stuffprob_4.log
chip4.magma
sex_4.repl
4000000
chip4.stuffprob
END_STUFFPROB
# Chip 5
stuffprob << END_STUFFPROB > stuffprob_5.log
chip5.magma
sex_5.repl
5000000
chip5.stuffprob
END_STUFFPROB
# Chip 6
stuffprob << END_STUFFPROB > stuffprob_6.log
chip6.magma
sex_6.repl
6000000
chip6.stuffprob
END_STUFFPROB
# Chip 7
stuffprob << END_STUFFPROB > stuffprob_7.log
chip7.magma
sex_7.repl
7000000
chip7.stuffprob
END_STUFFPROB
# Chip 8
stuffprob << END_STUFFPROB > stuffprob_8.log
chip8.magma
sex_8.repl
8000000
chip8.stuffprob
END_STUFFPROB
