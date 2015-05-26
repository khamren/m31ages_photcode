import pdb
import numpy as np

from runBasicDAOphot import runBasicDAOphot
from allfprep import allfprep


'''
Last edit - 5/26/2015, K. Hamren

Steps and Structure:
1. Run DAOPHOT to fit a PSF to the image
   code - runBasicDAOphot.py
   calls - DAOsuite, goodpsf

2. Create input for ALLFRAME by iteratively finding stars (SExtractor) and fitting/substracting stars (ALLSTAR)
   code - allfprep.py
   calls - none

3. Use PSF to measure magnitudes in image
   code - allframe_wrapper.py
   calls - allframe.sh

4. Run stuffprob/stuffprop.sh
5. Aperture corrections
6. Apply aperture correction, zeropoint, dust
7. convert pixels to RA/DEC
'''

def M31AGES_pipeline(image):

	
    runBasicDAOphot(image)
    allfprep(image, xoff, yoff)
