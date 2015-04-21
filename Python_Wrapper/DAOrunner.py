import os
import shutil
import numpy as np

from DAOsuite import * #imports runDAOPHOT1, runDAOPHOT2
from goodpsf import * #imports goodpsf


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

#-------------------------------------------------------------------------------------
    '''
    Step 1: Run DAOFIND, PICKPSF (runDAOPHOT1), filter those PSF sources (lstfilter)
            and run PSF (runDAOPHOT2)

    '''
    #copy {image}.opt file to daophot.opt for safety
    os.remove('daophot.opt')
    shutil.copy('%s.opt' %(image), 'daophot.opt')

    runDAOPHOT1(image) #Outputs coordinates (.coo), apertures (.ap), and psf list (.lst) files
    lstfilter(image) #Takes in {image}.lst, outputs {image}.lst1
    runDAOPHOT2(image) #Outputs .psf file and .psf.log file

#-------------------------------------------------------------------------------------
    '''
    Step 2: filter out bad PSF stars and construct a second version of PSF
            Functionality of the older code has been folded into goodpsf.py
            See that documentation for an explanation
    '''

    os.remove('%s.lst2' %(image))
    goodpsf('%s.lst1' %(image), '%s.psf.log' %(image), '%s.lst2' %(image))
    os.rename('%s.psf.log' %(image), 'LogFiles/%s.psf.log.V1' %(image))

    runDAOPHOT3(image)
    os.rename('%s.psf.log' %(image), 'LogFiles/%s.psf.log.V2' %(image))
#-------------------------------------------------------------------------------------              
    '''
    Step 3: Subtract neighbor stars and construct a final PSF

    '''

    extensions = ['.grp','.nst','a.fits','.lst2.chi','.lst3','.lst3.chi','.plst','.psf.log','.plst.chi','.psf.log']
    for e in extentions:
        os.remove('%s.%s' %(image.e))    

    runDAOPHOT4(image)
    goodpsf('%s.lst2' %(image), '%.psf.log' %(image), '%s.lst3' %(image))
    os.rename('%s.psf.log' %(image), 'LogFiles/%s.psf.log.V3' %(image))

    #Rerun PSF and goodpsf
    runDAOPHOT5(image) 


