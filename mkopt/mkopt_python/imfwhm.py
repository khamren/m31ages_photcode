
def imfwhm(inpt, fwhm, **kwargs):

    im = kwargs.get('im',False)
    ninpt = len(input)

    #load the input
    files, nfiles = loadinput(inpt, count = True)

    if (nfiles > 1) & im:
        print 'Multiple files AND input image. Using hte multiple files'
        inpim = False
    elif (nfiles < 1) & im:
        print 'Using the input image'
        inpim = im


    #starting ALLFWHM
    allfwhm = np.ones(nfiles)*99.99

    for f in files:
        fwhm = 99.99 #bad until proven good

        if not inpim: #if we're working with files
            if not os.path.isfile(f):
                print '%s NOT FOUND' %(f)
                #goto, skip

            hdu = pyfits.open(f)
            im = hdu[1].data
            head = hdu[1].header

        else: #if we're working with an input image
            im = inpim

        #We have an image
        nx, ny = np.size(im)
        satlim = max(im)
        try:
            saturate = head['SATURATE'] #need the number of pixels that are saturated, what is the keyword?
        except headerdoesnotexist:
               stuff

        satlim = satlim < max(im)
        satlim = satlim < 65000.

        #checking how many "good" pixels are there
        gdpix = im[(im  < satlim*0.95) & (im != 0.0)]
        if len(gdpix) < 2:
            print '%s DOES NOT HAVE ENOUGH GOOD PIXELS'
            return

        #compute sky level and sigma
        
        
            
        
                
