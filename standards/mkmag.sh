#!/bin/csh
#
#  SECOND STAGE : Concentric photometry (prepare for DAOGROW)
#
selstartot="/net/halo/bin/selstartot"
set image=${1}
selstartot << DONE
${image}a.tot
${image}.coord
3.
${image}.mag
DONE
