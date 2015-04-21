
'''
Read a typical DAOPHOT .lst file

INPUT - file name
OUTPUT - data + header
'''

def lstRead(infile):
    
    fin = open(infile,'r')
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
        id.append(int(idi))
        x.append(xi)
        y.append(yi)
        mag.append(magi)
        err.append(erri)
        sky.append(skyi)

    fin.close()
    data = (np.array(id), np.array(x), np.array(y), np.array(mag), np.array(err), np.array(sky))
    header = (head1, head2, head3)

    return data, header


def lstWrite(data, header, outfile):

    id, x, y, mag, err, sky = data
    head1, head2, head3 = header

    fout = open(outfile,'w')
    fout.write(head1)
    fout.write(head2)
    fout.write(head3)
    for i in indGood:
        fout.write('%7i %8.3f %8.3f %6.3f %6.3f %6.3f \n'%(id[i], x[i], y[i], mag[i], err[i], sky[i]))

    fout.close()
