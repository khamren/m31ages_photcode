import numpy as np

def lstfilter(image):

    #get appropriate files
    lstfile = '%s.lst' %(image)
    findfile = '%s.coo' %(image)
    outfile = '%s.lst1' %(image)

    #read .lst file, and grab the header
    fin = open(lstfile,'r')
    head1 = fin.readline()
    head2 = fin.readline()
    head3 = fin.readline()

    id = []
    x = []
    y = []
    mag = []
    err = []
    sky = []
    for f in fin:
        idi, xi, yi, magi, erri, skyi = map(float,f.strip().split())
        id.append(idi)
        x.append(xi)
        y.append(yi)
        mag.append(magi)
        err.append(erri)
        sky.append(skyi)

    fin.close()
    nstar = len(id)
    

    #read .coo file
    cid, sharp, rnd = np.genfromtxt(findfile, usecols = (0,4,5), unpack = True, skiprows = 3)
    nnstar = len(cid)

    #match the .coo and .lst file
    indMatch = [e[0] for e in enumerate(cid) if e[1] in id]
    #pull out objects from the .lst file that have acceptable sharp measurements
    indGood = [i for i in range(nstar) if (sharp[indMatch][i] > 0.3) & (sharp[indMatch][i] < 1.0)]

    #write out the new list file
    fout = open(outfile,'w')
    fout.write(head1)
    fout.write(head2)
    fout.write(head3)
    for i in indGood:
        fout.write('%7i %8.3f %8.3f %6.3f %6.3f %6.3f \n'%(id[i], x[i], y[i], mag[i], err[i], sky[i]))

    fout.close()
