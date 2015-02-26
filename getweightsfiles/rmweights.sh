##
# This is a companion script to getweights.sh used to put 
# together the input files (weights_?.inp) median "getweights".
#
# This script will open each file in vi and remove the 
# unnecessary text. 
#
# It will have a few error messages ... and will not put the
# parameters into one line, that must be done by hand (sorry).
#
# RLB -- 06/08/2007
##
# Chip 1
##
vi weights_1.inp << DONE
:%s/RE/
:w
:%s/FW/
:w
:%s/Clipped/
:%s/mean/
:%s/and/
:%s/median/
:%s/=/
:wq
<< DONE
##
# Chip 2
##
vi weights_2.inp << DONE
:%s/RE/
:w
:%s/FW/
:w
:%s/Clipped/
:%s/mean/
:%s/and/
:%s/median/
:%s/=/
:wq
##
# Chip 3
##
<< DONE
vi weights_3.inp << DONE
:%s/RE/
:w
:%s/FW/
:w
:%s/Clipped/
:%s/mean/
:%s/and/
:%s/median/
:%s/=/
:wq
<< DONE
##
# Chip 4
##
vi weights_4.inp << DONE
:%s/RE/
:w
:%s/FW/
:w
:%s/Clipped/
:%s/mean/
:%s/and/
:%s/median/
:%s/=/
:wq
<< DONE
##
# Chip 5
##
vi weights_5.inp << DONE
:%s/RE/
:w
:%s/FW/
:w
:%s/Clipped/
:%s/mean/
:%s/and/
:%s/median/
:%s/=/
:wq
<< DONE
##
# Chip 6
##
vi weights_6.inp << DONE
:%s/RE/
:w
:%s/FW/
:w
:%s/Clipped/
:%s/mean/
:%s/and/
:%s/median/
:%s/=/
:wq
<< DONE
##
# Chip 7
##
vi weights_7.inp << DONE
:%s/RE/
:w
:%s/FW/
:w
:%s/Clipped/
:%s/mean/
:%s/and/
:%s/median/
:%s/=/
:wq
<< DONE
##
# Chip 8
##
vi weights_8.inp << DONE
:%s/RE/
:w
:%s/FW/
:w
:%s/Clipped/
:%s/mean/
:%s/and/
:%s/median/
:%s/=/
:wq
<< DONE
