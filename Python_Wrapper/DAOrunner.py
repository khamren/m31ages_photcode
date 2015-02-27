import os
import shutil
import numpy as np

from DAOsuite import *


def main(image):

    #Check that all necessary files exist
    reqFiles = ['%s.fits' %(image), '%s.opt' %(image), 'photo.opt', 'apcor.opt', '%s.als.opt' %(image)]
    for rf in reqFiles:
        if os.path.isFile(rf):
            continue
        else:
            print 'ERROR: %s cannot be found' %(rf)
            return

    print "starting photometry on %s.fits" %(image)

    #Delete existing files
    extentions = ['.log','.coo','.ap','.lst','.lst1','.psf','.nei','.psf.log']
    for e in extentions:
        os.remove('%s.%s' %(image.e))

    #create log file
    logf = open('%s.log' %(image), 'w')


#-------------------------------------------------------------------------------------
    #Step 1: Construct PSF
    logf.write("=================================== \n")
    logf.write("== Step 1 : Construct PSF ver. 1 == \n")
    logf.write("=================================== \n")
    logf.write("\n")

    #copy {image}.opt file to daophot.opt for safety
    os.remove('daophot.opt')
    shutil.copy('%s.opt' %(image), 'daophot.opt')

    runDAOPHOT1(image)
    lstfilter(image)
    runDAOPHOT2(image)

#-------------------------------------------------------------------------------------
    #Step 1: filter out bad PSF stars and construct a second version of PSF

    os.remove('%s.lst2' %(image))
    os.remove('%s.lst1.chi' %(image))
    os.remove('%s.lst2.chi' %(image))

    logf.write("=================================== \n")
    logf.write("== Step 1 : Construct PSF ver. 1 == \n")
    logf.write("=================================== \n")
    logf.write("\n")

    ln1='grep -n "Profile errors:" %s.psf.log | sed 's/:/ /g' | awk '(NR==1){print $1+2}''
    

    
