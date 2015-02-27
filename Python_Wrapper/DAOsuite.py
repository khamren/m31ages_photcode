import os
import re
import subprocess
import numpy as np

daophot = "/net/halo/bin/daophot"
allstar = "/net/halo/bin/allstar"

def updateScript(script, image):
    
    with open(script) as fin:
        lines = fin.readlines()

    for i in range(len(lines)):
        if 'set image' in lines[i]:
            torep = re.split('=',lines[i])[-1]
            break

    lines[i] = lines[i].replace(torep, '%s \n' %(image))

    with open(script) as fout:
        for l in lines:
            fout.write(l)

            
def runDAOPHOT1(image):
    script = 'daophotscript1'
    updateScript(script, image)

    subprocess.Popen('./daophotscript1', shell=True)


def runDAOPHOT2(image):
    script = 'daophotscript2'
    updateScript(script, image)

    subprocess.Popen('./daophotscript2', shell=True)
	
	
