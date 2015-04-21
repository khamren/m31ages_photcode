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
    #Runs FIND, PHOTOMETRY, and PICKPSF
    script = 'daophotscript1'
    updateScript(script, image)

    subprocess.Popen('./daophotscript1', shell=True)


def runDAOPHOT2(image):
    # Runs PSF with input list .lst1
    script = 'daophotscript2'
    updateScript(script, image)

    subprocess.Popen('./daophotscript2', shell=True)
	
def runDAOPHOT3(image):
    # Runs PSF with input list .lst2
    script = 'daophotscript3'
    updateScript(script, image)

    subprocess.Popen('./daophotscript3', shell=True)	

def runDAOPHOT4(image):
    # Runs GROUP, NSTAR, SUBSTAR
    script = 'daophotscript4'
    updateScript(script, image)

    subprocess.Popen('./daophotscript4', shell=True)

def runDAOPHOT5(image):
    # Runs PSF with input .lst2 on ${image}a (post substar)
    script = 'daophotscript5'
    updateScript(script, image)

    subprocess.Popen('./daophotscript5', shell=True)

def runDAOPHOT6(image):
    # Runs PSF with input .lst3 on ${image}a (post substar)
    script = 'daophotscript6'
    updateScript(script, image)

    subprocess.Popen('./daophotscript6', shell=True)

def runDAOPHOT7(image):
    #Runs PHOTOMETRY on {image}a
    script = 'daophotscript7'
    updateScript(script, image)

    subprocess.Popen('./daophotscript7', shell=True)
