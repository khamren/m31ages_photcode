import numpy as np

from lstManip import lstRead, lstWrite

def lstfilter(image):

    #get appropriate files
    lstfile = '%s.lst' %(image)
    findfile = '%s.coo' %(image)
    outfile = '%s.lst1' %(image)

    #read .lst file, and grab the header
    lData, lHeader = lstRead(lstfile)
    id, x, y, mag, err, sky = lData
    nstar = len(id)
    
    #read .coo file
    cid, sharp, rnd = np.genfromtxt(findfile, usecols = (0,4,5), unpack = True, skiprows = 3)
    nnstar = len(cid)

    #match the .coo and .lst file
    indMatch = [e[0] for e in enumerate(cid) if e[1] in id]
    #pull out objects from the .lst file that have acceptable sharp measurements
    indGood = [i for i in range(nstar) if (sharp[indMatch][i] > 0.3) & (sharp[indMatch][i] < 1.0)]

    #write out the new list file
    outData = (id[indGood], x[indGood], y[indGood], mag[indGood], err[indGood], sky[indGood])
    lstWrite(outData, lHeader,outfile)
