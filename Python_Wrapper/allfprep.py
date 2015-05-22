import os
import shutil


'''
The purpose of this code is to replace allfprep.cl. 
1- run SExtractor
2- fit/substract stars using the PSF in ALLSTAR
3- find stars again in subtracted image
4- build master list formatted for input into ALLFRAME
'''

def main(image, xoff, yoff):

    done = False
    while not done:

        #run source extractor
        command = "sex allf.fits -c default.sex"
        os.system(command)

        toDelete = ["sex","sex1","sex2"]
        for t in toDelete:
            os.remove('%s.fits' %(t))

        shutil.copy('sex.cat', 'sex1.cat')
        
