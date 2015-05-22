# m31ages_photcode
Code for the M31AGES Photometry

Our starting point is the SPLASH photometry. The SPLASH photometry was taken with KPNO 4-meter+MOSAIC and each of the 8 chips in MOSAIC were reduced independently. The SPLASH photometry was taken in 3 filters: M (like V), T2 (=I), and DDO51, where the latter is a intermediate band filter inside of M. The images were deep -- 1x 15 min, 2x 15 min and 3x 30 min in M, T2, DDO51 -- and so the sky backgrounds were large (8000 to 10000 counts!). In this limit, DAOPhot did not always perform well. So, I had a piece-wise photometry pipeline broken into small chunks so I could diagnose and resolve problems as they occured. Also, I am not a very good code writer and adopted things from previous graduate students/postdocs.

Here's how the codes should be ordered:

We assume that the images are in units of counts, are split into chips, and are processed. 

1. __mkopt.pro (IDL):__ Fenerates the OPT files for command line DAOPHOT & ALLSTAR

2. __autopsf.uva2.sh (Shell):__  (Calls: daophot, allstar, lstfilter, goodpsf); copies the OPT file; runs basic daophot, exits after a psf list; uses goodpsf and lstfilter to edit the psf list removing non-star like sources; finishes daophot doing a run through neighbor subtraction; runs allstar 

3. __daophotassess.sh (Shell):__ reads log files and produces a messy report of how daophot went, then MANUAL INTERVENTION to run PSF by hand (or whatever) orreprox the images (or whatever)

4. __daomatch.sh (Shell), daomaster.sh (Shell):__ match sources using pixel coordinates, makes an output catalog then MANUAL INTERVENTION: check the prelim CMD and image stuff, redo whatever is necessary

5. __getweights_k.sh (Shell): [[Isn't there a WRAPPPER?]]__ computes S/N in images + flux ratios of RGB stars to determine an optimal weighting scenario to co-add each of the images for each filter

6. __alignncombine.cl (IRAF):__ Uses the xy shifts from daomatch and a star list (either made manually or using the XY coordinates of the PSF stars from the primary image) to align the frames. Then combines the frames into a single image using the weights. The output file is called "allf"

7. __mkopt.opt (IDL) + getpsf.sh (Shell):__ preps for and runs daophot to fit a PSF to the co-added image

8. __allfprep.cl (IRAF):__ Finds stars in the co-added image using SExtractor, fits/subtracts the stars using the PSF in ALLSTAR. Finds stars (Again) in the subtracted image. Builds one master list (keeping track of star IDs) that is formatted for input into ALLFRAME. __CALLS: daophot, allstar, source extractor, image display (interactive)__

9. __allframe.sh (Shell):__ uses the PSFs for each frame to simultaneously measure magnitudes in all frames using our master source list. __CALLS: allframe__

10. __daomaster_allf.sh (Shell):__ confirms the shifts (that we already know) using the catalogs output from allframe and produces the output files we need (.tfr).

11. __makemag (fortran) / runmakemag.sh (shell):__ Uses the tfr file from daomaster to match all of the sources in each catalog. Averages the chi/sharp across filters. __MODIFICATION: Want to keep the chi/sharp from each frame, not just the average value.__

12. __getoverlap.txt (python?) / badlines.sh (shell):__ goes through the makemag file to look for large chi/sharp values that will bleed through their alloted spaces from the fortran code. this is not always necessary.

13. __stuffprob (fortran) / runstuffprob.sh (shell):__ Uses the make mag file and a log file from allfprep to pull the stellaricity parameters from sextractor (flag, prob) and input into an output catalog

14. __Aperture Corrections; preapcor.sh (shell), apcor.sh (shell), daogrow (fortran), mkdel (fortran), <another>.cl (IRAF):__ make an image with the PSF neighbor stars subtracted; does aperture photometry of the PSF stars using concentric magnitudes; solves the curve of growth for the infinite aperture correction; organizes the outputs; finds a final value;

15. __Photometric Standard Stars (many codes):__  __MODIFICATION: WE have photometric zero points in the header. NO tests for color corrections ... (yet).__

16. __magma.pro or magma.rlb.pro (IDL):__  Applies the photometric transformation equations (15), applies the aperture correction (14), iteratively solves for the color and magnitude of the star using these inputs for a self consistent solution. Combines multiple exposures of the same star (if input) and has the option to bootstrap non-photometric fields to photometric fields. 

17. __astrometry (Shell/IRAF):__ Updates the WCS and uses those headers to convert xy coordinates to ra/dec __MODIFICATIONS: Will need a new code. The image headers are already updated using the 2MASS.__

18. __mkchartform.pro (IDL):__ does some book-keeping and pulls extinction from Schelgal maps; does conversion from E(B-V) to the proper colors/magnitudes; computes a radial distance from the dSph/M31. 


Questions

1. 
2. DAOrunner.py performs all of the steps in autopsf.uva2.sh
3. What is this?
4. daomatch.sh missing from git
5. 
6. alignncombine.cl missing from git
7. getpsf.sh seems identical to autopsf.uva2.sh, why is this a separate code
8. Why have we suddenly switched to using SExtractor rather than DAOFIND. ANSWER - speed!!
9. 


Python'd version
1. - 6. SKIP
7. Start here. getpsf.sh == autopsf.uva2.sh
8. yes
9. yes
10. SKIP
11. SKIP (we want chi and sharp for J and K independently)
12. SKIP
13. yes (and modify), because we have a lot of galaxies and prob will help
14. yes
15. SKIP
16. heavily modify - just apply the aperture correction, zeropoint, E(B-V) --> E(J-K)
17. modify - convert pixels to RA/DEC

