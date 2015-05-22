import os
import shutil
import numpy as np

from goodpsf import goodpsf
from DAOsuite import DAOsuite

'''
Last edit: 4/21/2015 - K. Hamren

Purpose: getPSF runs daophot (DAOFIND, PICKPSF, PSF, ALLFRAME) on an input image 
         to find sources, create a PSF, and perform photometry

Input: image name (without extension)
Output: coordinates file (.coo)
        lists of PSF stars (.lst, .lst1, .lst2, .lst3), with .lst3 being the FINAL PSF list
        photometry (.als)
        
'''
def runBasicDAOphot(image):

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
    extentions = ['log','coo','ap','lst','lst1','psf','nei','psf.log']
    for e in extentions:
        os.remove('%s.%s' %(image.e))

    dao = DAOsuite(image)
#-------------------------------------------------------------------------------------
    '''
    Step 1: Run DAOFIND, PICKPSF (runDAOPHOT1), filter those PSF sources (lstfilter)
            and run PSF (runDAOPHOT2)

    '''
    #copy {image}.opt file to daophot.opt for safety
    os.remove('daophot.opt')
    shutil.copy('%s.opt' %(image), 'daophot.opt')

    dao.runDAO('daophotscript1') #Outputs coordinates (.coo), apertures (.ap), and psf list (.lst) files
    lstfilter(image) #Takes in {image}.lst, outputs {image}.lst1
    dao.runDAO('daophotscript2') #Outputs .psf file and .psf.log file

#-------------------------------------------------------------------------------------
    '''
    Step 2: filter out bad PSF stars and construct a second version of PSF
            Functionality of the older code has been folded into goodpsf.py
            See that documentation for an explanation
    '''

    os.remove('%s.lst2' %(image))
    goodpsf('%s.lst1' %(image), '%s.psf.log' %(image), '%s.lst2' %(image))
    os.rename('%s.psf.log' %(image), 'LogFiles/%s.psf.log.V1' %(image))

    dao.runDAO('daophotscript3')
    os.rename('%s.psf.log' %(image), 'LogFiles/%s.psf.log.V2' %(image))
#-------------------------------------------------------------------------------------              
    '''
    Step 3: Subtract neighbor stars and construct a final PSF

    '''

    extensions = ['.grp','.nst','a.fits','.lst2.chi','.lst3','.lst3.chi','.plst','.psf.log','.plst.chi','.psf.log']
    for e in extentions:
        os.remove('%s%s' %(image.e))    

    dao.runDAO('daophotscript4')
    goodpsf('%s.lst2' %(image), '%.psf.log' %(image), '%s.lst3' %(image))
    os.rename('%s.psf.log' %(image), 'LogFiles/%s.psf.log.V3' %(image))

    #Rerun PSF and goodpsf
    dao.runDAO('daophotscript5')

 #-------------------------------------------------------------------------------------              
    '''
    Step 4: perform PSF photometry with ALLSTAR

    '''
    extensions = ['.als','s.fits']
    for e in extensions:
        os.remove('%s%s' %(image, e))
    os.remove('allstar.inp')

    shutil.copy('%s.als.opt' %(image), 'allstar.inp')
    with open('allstar.inp', 'a') as file:
        file.write('%s \n' %(image))
        file.write('%s.psf \n' %(image))
        file.write('%s.ap \n' %(image))
        file.write('%s.als \n' %(image))
        file.write('%ss \n' %(image))

    dao.runALLFRAME(log = '%s.log' %(image))

#-------------------------------------------------------------------------------------              
    '''
    Step 5: photometry with aperture correction
    '''

    extensions = ['a.log','a.ap','a.als','as.fits']
    for e in extensions:
        os.remove('%s%s' %(image, e))
    os.remove('allstar.opt')
    

    dao.runDAO('daophotscript7')

    shutil.copy('%s.als.opt' %(image), 'allstar.opt')
    shutil.copy('%s.als.opt' %(image), 'allstar.inp')
    with open('allstar.inp', 'a') as file:
        file.write('%sa \n' %(image))
        file.write('%s.psf \n' %(image))
        file.write('%sa.ap \n' %(image))
        file.write('%sa.als \n' %(image))
        file.write('%sas \n' %(image))

    dao.runALLFRAME(log = '%sa.log' %(image))

    os.remove('allstar.inp')
    os.remove('%sas.fits' %(image))
