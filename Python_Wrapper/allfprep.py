import os
import shutil


'''
The purpose of this code is to replace allfprep.cl. 
1- run SExtractor
2- fit/substract stars using the PSF in ALLSTAR
3- find stars again in subtracted image
4- build master list formatted for input into ALLFRAME
'''

def line_prepender(lines, infile, outfile):
    with open(outfile, 'w') as f:
        for l in lines:
            f.write(l)
        with open(infile,'r') as fi:
            l = fi.readline()
            f.write(l)
        
def allfprep(image, xoff, yoff):

    toDelete = ["sex","sex1","sex2"]
    for t in toDelete:
        os.remove('%s.fits' %(t))
        
    #run source extractor
    command_sex = "sex allf.fits -c default.sex"
    os.system(command_sex)
    
    #Right now allfprep.cl displays allf.fits, edits SExtractor to contain only X and Y (for TVMARK), marks stars on the image
    #deletes the tvm file, and asks if things are ok
    
    #We want to just pick SExtractor parameters that work well.
    
    shutil.copy('sex.cat', 'sex1.cat') #Line 80 of allfprep.cl
    
    with open("allf.als") as file:
        head = []
        for i in range(3):
            head.append(file.readline())
            
    line_prepender(head, "sex1.cat","sex1.tmp")
    os.shutil("sex1.tmp","sex1.cat")
    
    #Create input file for Allstar
    with open('tmp.inp','w') as tmp_inp:
        tmp_inp.write("     \n")
        tmp_inp.write("allf \n")
        tmp_inp.write('%s.psf \n' %(image))
        tmp_inp.write('sex1.cat \n')
        tmp_inp.write('sex1.als \n')
        tmp_inp.write('sex1s \n')
                    
    command_als = 'allstar < tmp.inp > allfprep.log'
    os.system(command_als)
    
    os.remove('tmp.inp')
    
    #Run SExtractor on subtracted image from Allstar
    command = 'sex sex1s.fits -c default.sex'
    os.system(command)
    os.shutil('sex.cat','sex2.cat') #Line 111 of allfprep.cl
    
    line_prepender(head, "sex2.cat", "sex2.tmp")
    os.shutil("sex2.tmp","sex2.cat")
        
    #Both here and after the last copy we are putting the first 3 lines of the .als file in front of sex#.cat
    #Make input file for daophot
    #Run offset in daophot to offset the star ID in star subtracted catalog, append new list to old list
    with open('tmp.inp','w') as tmp_inp:
        tmp_inp.write("offset \n")
        tmp_inp.write("sex2.cat \n")
        tmp_inp.write("100000,0,0,0 \n")
        tmp_inp.write("sex2.off \n")
        tmp_inp.write("append \n")
        tmp_inp.write("sex1.als \n")
        tmp_inp.write("sex2.off \n")
        tmp_inp.write("sex.cmb \n")
        

    command_dao = "daophot < tmp.inp >> allfprep.log"
    os.system(command_dao)

    os.remove('tmp.inp')
        
    #Make input file for allstar
    with open('tmp.inp','w') as tmp_inp:
        tmp_inp.write("     \n")
        tmp_inp.write("allf \n")
        tmp_inp.write('%s.psf \n' %(image))
        tmp_inp.write('sex.cmb \n')
        tmp_inp.write('sex2.als \n')
        tmp_inp.write('sex2s \n')
        
    command_als2 = 'allstar < tmp.inp >> allfprep.log'
    os.system(command_als2)

    #Make input file for DAOPHOT
    with open('tmp.inp','w') as tmp_inp:
        tmp_inp.write("offset \n")
        tmp_inp.write("sex2.als \n")
        tmp_inp.write("0, %s, %s,0  \n" %(xoff1, yoff1))
        tmp_inp.write("allf.mag \n")
        tmp_inp.write("    \n")
        tmp_inp.write("offset \n")
        tmp_inp.write("sex1.cat \n")
        tmp_inp.write("0,0,0,0 \n")
        tmp_inp.write("sex1.off \n")
        tmp_inp.write("   \n")
        tmp_inp.write("append \n")
        tmp_inp.write("sex1.off \n")
        tmp_inp.write("sex2.off \n")
        tmp_inp.write("sex.repl \n")
        tmp_inp.write("   \n")
        

    os.system(command_dao)
    os.remove('tmp.inp')
