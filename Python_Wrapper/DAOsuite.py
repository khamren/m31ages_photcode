import os
import re
import subprocess
import numpy as np

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

class DAOsuite:

    def __init__(self, image):

        daophot = "/net/halo/bin/daophot"
        allstar = "/net/halo/bin/allstar"
        
        self.image = image

    def runDAO(self, script):
        excScript = './%s' %(script)
        updateScript(script, self.image)

        subprocess.Popen(excScript, shell = True)

    def runALLFRAME(self, **kwargs)
        log = kwargs.get('log', '& /dev/null')
        command = 'allstar < allstar.inp >> %s' %(log)

        subprocess.call(command, shell = True)

