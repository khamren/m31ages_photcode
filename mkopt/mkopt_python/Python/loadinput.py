import glob
import numpy as np

def loadinput(inpt, **kwargs):

    comment = kwargs.get('comment','#')
    
    if inpt[0] == '@': #list input
        lst = np.genfromtxt(inpt, dtype = 'str', comment = comment)
        nlst = len(lst)

    elif inpt[0] == '*': #globbed list of files
        lst = glob.glob(inpt)
        nlst = len(lst)

    elif np.size(inpt) == 1: #a single file
        lst = inpt
        nlst = 1
        
    else:
        #an array of files
        ninpt = len(inpt)
        lst = []
        for i in inpt:
            if i[0] == '*':
                sublst = glob.glob(i[0])
                for l in sublst:
                    lst.append(l)
            else:
                lst.append(i)
        nlst = len(lst)

    count = kwargs.get('count',False)

    if count:
        return lst, nlst
    else:
        return lst

        
                
        
