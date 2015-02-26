import os
import pidly
import numpy as np

from ConvenienceFuncs import resistant_mean

Problems with this code:
1- the values for the raw images and the values for the images with the sky background added will be quite different. Thus, I do not know what default values to put.
2- No saturate keyword in NIR fits files
3- No readnoise keyword in NIR fits files


'''
This code makes the opt files for FITS files to be used with DAOPHOT and ALLSTAR
It was ported into python from mkopt_hst.pro

; INPUTS:
;  input   Input files. Three formats can be used (1) Name of file
;          with a list of input filenames.  Must start with an '@';
;          (2) A name with wildcard characters, such as '*';
;          (3) An array of filenames.
;  hilimit The saturation upper limit, 64,000 by default.
;  /stp    Stop at the end of the program.
;  ONE OF THE TWO MUST BE SET:
;  /eneg   Image units are electrons
;  /cts    Image unts are in counts

; OUTPUTS:
;  Makes .opt and .als.opt files for each FITS file

; (1) DAOPHOT parameters
;
; LO    : Low good datum (15. works fine on most imags)
; TH    : Threshold (3.5 works fine)
; LS,HS : Low and high sharpness (default : 0.2 - 1.0)
; LR,HR : Low roundness and high roundness (default : -1.0 - 1.0)
; WA    : Watch progress 
; VA    : Variable PSF
; AN    : Analytic model PSF
; EX    : Extra PSF cleaning passes
; PE    : Percent error
; PR    : Profile error

; (2) ALLSTAR parameters
;
; CR    : Clipping range (leave it)
; CE    : Clipping exponent (leave it)
; MA    : Maximum group size
; RED   : Redetermine centroid (0 = no, 1 = yes)

; Frame-specific parameters.
;
; GA    : gain (e/ADU)
; RD    : readout noise (e)
; RE    : readout noise (ADU)
; FW    : FWHM
; HI    : hi good datum in ADU - saturation level
; FI    : fitting radius
; PS    : PSF radius
; IS,OS : inner and outer sky annalus

'''

#initialize values
LO =  15.0
TH =  3.5
LS =  0.2
HS =  1.0
LR = -1.0
HR =  1.0
WA = -2
VA =  2
AN = -6
EX =  5
PE =  0.75
PR =  5.00
CR =  2.5
CE =  6.0
MA = 50.
RED = 1.0
WA2 = 0.0
RE = 1.0

#initialize the ability to call IDL
idl = pidly.IDL()

def mkopt(inpt, **kwargs):

#======= Parsing the input and keywords ===================
    hilimit = kwargs.get('hilimit', 2.5E4)
    
    #Default image units are counts
    eneg = kwargs.get('eneg', False)
    cts = kwargs.get('cts', True)

    if eneg & cts:
        print 'ERROR -- the image cannot have two sets of units'
        return

    #Getting file list given different inputs
    if inpt[0] == '@': #input was a list file

        files = np.genfromtxt(inpt[1:], dtype = 'str')
    else:
        if np.size(input) > 1: #input was an array of filenames
            files = inpt
        else:
            print 'ERROR -- Unrecognizable input \n Please re-run with either a list file or an array of files'
            return

    #Check that files actually exist
    files = [f for f in files if os.path.isfile(f)]
    nfiles = len(files)

    if len(files) == 0:
        print 'ERROR: none of these files exist'
        return

    nofiles = [f for f in files if not os.path.isfile(f)]
    print 'files that do not exist: '
    print nofiles

#========== Getting the Data =======================
    gaarr = []
    rdarr = []
    fwarr = []
    hiarr = []

    filarr = [re.split('/',f)[-1][:-5] for f in files]

    for fle in files:
    
        head = pyfits.getheader(fle)
        im = pyfits.getdata(fle)

        #Get filename, gain, readnoise, FWHM, Hi-limit
    
        #get gain
        if cts:
            gain = head['GAIN']
        else: #if eneg
            gain = 1.0

        #get readnoise
        #rdnoise = head['READNOISE KEYWORD']

        #get fwhm using D. Nidever's iwfm.pro
        idl.pro('iwfm, fwhm', fle, im=im)
        fwhm = idl.fwhm

        #get Hi-limit
        hilim = max(im.flatten()) - 0.20*max(im.flatten())
        hilim = np.floor(hilim/100.)*100

        #get saturation limit
        #saturate = head['SATURATE']
        #if saturate == 0:
        #    staturate = hilimit

        #minimum of all saturation levels
        hi = max(lolimit,(min(saturate-4000.0, hilimit)))

        #append data to arrays
        gaarr.append(gain)
        #rdarr.append(rdnoise)
        fwarr.append(fwhm)
        hiarr.append(hi)


#===== Use median value for each mosaic frame ===========

    filebase = [re.split('_',f)[0] for f in filarr]
    fils = list(set(filebase))
    nui = len(fils)

    #copy the array to avoid editing fwhm
    fwarr_orig = np.copy(fwhm)
    for i in range(nui):

        medfw = np.median(fwarr[(filebase == fils[i]) & (fwarr < 40)])
        if len(medfw) > 0:
            mnfw, sigma_mnfw = resistant_mean(fwarr[g], 2.0)
            fwarr[filebase == fils[i]] = mnfw
        else:
            fwarr[filebase == fils[i]] = 99.99
            mnfw = 99.99
            print 'NO GOOD FWHM FOR %s' %(files[i])

        print 'FWHM for %s = %7.3f' %(files[i], mnfw)

#======== MAKING THE OPT FILES =================
    for i in range(nfiles):
        fle = filarr[i]
        GA = gaarr[i]
        #RD = rdarr[i]
        FW = fwarr[i]
        HI = hiarr[i]

        #Calculating some things
        #RE = RD/GA
        FI = FW
        PS = min((4.0*FW),51)       # daophot won't accept anything higher than 51
        IS = FI - 1.0
        OS = PS + 1.0

        #Write OPT parameter file
        outarr = [RE,GA,LO,HI,FW,TH,LS,HS,LR,HR,WA,FI,PS,VA,AN,EX,PE,PR]
        anotarr = ['RE','GA','LO','HI','FW','TH','LS','HS','LR','HR','WA','FI','PS','VA','AN','EX','PE','PR']
        out = ['%s = %8.2f' %(a, o) for a, o in zip(anotarr)]
        np.savetxt('%s.opt' %(fle), out, fmt = '%s')

        #Write ALLSTAR paramter file
        outarr2 = [FI,IS,OS,RED,WA2,PE,PR,CR,CE,MA]
        anotarr2 = ['FI','IS','OS','RE','WA','PE','PR','CR','CE','MA']
        out2 = ['%s = %8.2f' %(a, o) for a, o in zip(anotarr2, outarr2)]
        np.savetxt('%s.als.opt' %(fle), out2, fmt = '%s')

        
                        

    
    
    

    
