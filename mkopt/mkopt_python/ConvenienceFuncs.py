import numpy as np
from scipy.stats import sigmaclip


def resistant_mean(arr, sigLim):

    clipArr, junk, junk = sigmaclip(arr, low = sigLim, high = sigLim)
    mn = np.mean(clipArr)
    sigma = np.std(clipArr)

    return mn, sigma
