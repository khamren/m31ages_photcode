from __future__ import division
from lstManip import lstRead, lstWrite
import numpy as np

'''
This is a rewrite of the fortran program goodpsf.f
It eliminates the need to first go through and replace instances of saturated and defective prior to running this code

SYNTAX:
goodpsf.goodpsf(psfFile, chiFile, outFile, optional keywords)

OPTIONAL KEYWORDS
nmax = maximum number of stars. Residual from the fortran code, it doesn't appear here
minchi = the minimum CHI value allowed for a PSF star. Can be changed.
                                                                                     
'''

def goodpsf(*args, **kwargs):

    if len(args) != 3:
        print 'ERROR, input format is goodpsf(psfFile, chiFile, outFile, kwargs)'
        return                                     

    psfFile, chiFile, outFile = args
    nmax = kwargs.get('nmax', 1000)
    minchi = kwargs.get('minchi', 0.5)

    #Read in psfFile (.lst)
    lData, lHeader = lstRead(psfFile)
    id, x, y, mag, err, sky = lData
    nstar = len(id)

    #Read in CHI file (.psf.log)
    cfileIn = open(chifile,'r').read()
    cfile = cfileIn.split('\n')

    #Find the region where the numbers are and pull that out
    start, stop = [e[0] for e in enumerate(cfile) if ('Profile errors:' in e[1]) | ('File with PSF stars and neighbors' in e[1])]
    subFile = cfile[start+1: stop-1]

    iid = []
    chi = []
    flag = []

    for line in subFile:
        sub = line.strip()
        for s in sub:
            if len(sub) > 0: 
                try: #Is this number an integer? That would make it the id
                    itst = int(s)
                    iid.append(itst)
                except ValueError:
                    try: #Is this number a float? That would make it the chi
                        ftst = float(s)
                        chi.append(ftst)
                    except ValueError:
                        ni = len(iid)
                        nc = len(chi)
                        nf = len(flag)
                        if nc < ni: #If we're missing a chi value, then this string must be a crappy chi indicator
                            chi.append(1.000)
                        elif (nc == ni) & (nf < nc): #If we are not missing a chi, then this string must be a flag
                            flag.append(s)
                        else:
                            print 'ERROR'
        nc = len(chi)
        nf = len(flag)
        if nf < nc: #Put in blank flags where none exist
            flag.append('')

    flag = np.array(flat)
    chi = np.array(chi)
    iid = np.array(iid)

    #Trim star list
    goodInd = (flag != '?') & (flag != '*') & (chi < minchi)
    matchInd = [e[0] for e in enumerate(id) if int(e[1]) in iid[goodInd]]

    #Output new .lst file
    out = (id[matchInd],x[matchInd],y[matchInd],mag[matchInd],err[matchInd],sky[matchInd]))
    lstWrite(out, lHeader, outfile)

    

    
