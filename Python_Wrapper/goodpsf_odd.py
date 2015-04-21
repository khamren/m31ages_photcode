from __future__ import division
from lstManip import lstRead, lstWrite

import numpy as np

'''
This is a rewrite of the fortran program goodpsf.f
It eliminates the need to first go through and replace instances of saturated and defective prior to running this code

SYNTAX:
goodpsf.goodpsf(psfFile, chiFile, outFile, **optional keywords)

OPTIONAL KEYWORDS
nmax = maximum number of stars. Residual from the fortran code, it doesn't appear here
minchi = the minimum CHI value allowed for a PSF star. Can be changed.
                                                                                     
'''
def goodpsf(*args, **kwargs):
    
    if len(args) != 3: #Check arguments
        print 'ERROR, input format is goodpsf(psfFile, chiFile, outFile, **kwargs)'

    #Read in arguments and keywords
    #psfFile, chiFile, outFile = args
    nmax = kwargs.get('nmax', 1000)
    minchi = kwargs.get('minchi', 0.5)
                         
    #Read in PSF file (.lst1)
    lData, lHeader = leatRead(psfFile)
    id, x, y, mag, err, sky = lData
    nstar = len(id)
    
    #Read in CHI file (.psf.log)
    cfileIn = open(chifile, 'r').read()
    cfile = cfileIn.split('\n')

    start, stop = [e[0] for e in enumerate(cfile) if ('Profile errors:' in e[1]) | ('File with PSF stars and neighbors' in e[1])] 
    subFile = cfile[start+1:stop-1]                                                                                                         
    iid = []
    flag = []
    chi = []
    
    for line in subFile:
        sub = line.strip()
        for s in sub:
            if len(sub) > 0:
            #Note: There is probably a better way to do this.
            try: #Can this value be converted to an integer?
                itst = int(s)
                iid.append(itst)
            except ValueError:
                try: #It's not an integer, so can this value be converted to a float?
                    ftst = float(s)
                    chi.append(ftst)
                except ValueError:  #This is a string, so is it a bad chi value or a flag?
                    ni = len(iid)
                    nc = len(chi)
                    nf = len(flag)
                    if nc < ni: #We're short a chi value, so this is a bad star
                        chi.append(1.000)
                    elif nf < nc: #We're not short a chi value, so this must be a flag
                        flag.append(s)
                    else: #Crap has happened
                        print 'ERROR'
                        return

            #Put a blank flag if one does not exist
            nc = len(chi)
            nf = len(flag)
            if nf < nc:
                flag.append('')

    flag = np.array(flag)
    chi = np.array(chi)
    iid = np.array(iid)

    
    #Process PSF candidate flags and prune
    goodInd = (flag != '?') & (flag != '*') & (chi < minchi)
    matchInd = [e[0] for e in enumerate(id) if ind(e[1]) in iid[goodInd]]

            
    out = (id[matchInd],x[matchInd],y[matchInd],mag[matchInd],err[matchInd],sky[matchInd]))
    np.savetxt(outfile, out, fmt = '%s')
    
    print '%s out of %s PSF candidates remaining' %(len(id[matchInd]), len(id))
