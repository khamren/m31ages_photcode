# m31ages_photcode
Code for the M31AGES Photometry

Our starting point is the SPLASH photometry. The SPLASH photometry was taken with KPNO 4-meter+MOSAIC and each of the 8 chips in MOSAIC were reduced independently. The SPLASH photometry was taken in 3 filters: M (like V), T2 (=I), and DDO51, where the latter is a intermediate band filter inside of M. The images were deep -- 1x 15 min, 2x 15 min and 3x 30 min in M, T2, DDO51 -- and so the sky backgrounds were large (8000 to 10000 counts!). In this limit, DAOPhot did not always perform well. So, I had a piece-wise photometry pipeline broken into small chunks so I could diagnose and resolve problems as they occured. Also, I am not a very good code writer and adopted things from previous graduate students/postdocs.

Here's how the codes should be ordered:

We assume that the images are in units of counts, are split into chips, and are processed. 

1. _mkopt.pro (IDL)_
--> this generates the OPT files for command line DAOPHOT & ALLSTAR

2. _[[CHECK THE NAME]].sh (Shell)_  (Calls: daophot, allstar, lstfilter, goodpsf)

--> copies the OPT file

--> runs basic daophot, exits after a psf list

--> uses goodpsf and lstfilter to edit the psf list removing non-star like sources

--> finishes daophot doing a run through neighbor subtraction

--> runs allstar 

3. _daophotassess.sh (Shell)_

--> reads log files and produces a messy report of how daophot went

--> MANUAL INTERVENTION: run PSF by hand (or whatever), reprox the images (or whatever)


4. daomatch.sh (Shell), daomaster.sh (Shell)

--> match sources using pixel coordinates, makes an output catalog 

--> MANUAL INTERVENTION: check the prelim CMD and image stuff, redo whatever is necessary

5. _getweights_k.sh (Shell)_

--> computes S/N in images + flux ratios of RGB stars to determine an optimal
 weighting scenario to co-add each of the images for each filter

6. _alignncombine.cl (IRAF)_

--> Uses the xy shifts from daomatch and a star list (either made manually or using the XY coordinates of the PSF stars from the primary image) to align the frames. Then combines the frames into a single image using the weights. The output file is called "allf"

7. mkopt.opt (IDL) + getpsf.sh (Shell) 

--> preps for and runs daophot to fit a PSF to the co-added image

8. allfprep.cl (IRAF) 

--> Finds stars in the co-added image using SExtractor, fits/subtracts the stars using the PSF in ALLSTAR. Finds stars (Again) in the subtracted image. Builds one master list that is formatted for input into ALLFRAME.

calls: daophot, allstar, source extractor, image display (interactive)

9. allframe.sh (Shell)

--> uses the PSFs for each frame to simultaneously measure magnitudes in all frames using our master source list.

calls: allframe

10. daomaster_allf.sh (Shell)

--> confirms the shifts (that we already know) using the catalogs output from allframe and produces the output files we need (.tfr).

11. makemag (fortran) / runmakemag.sh (shell)

--> Uses the tfr file from daomaster to match all of the sources in each catalog. Averages the chi/sharp across filters.

MODIFICATION: Want to keep the chi/sharp from each frame, not just the average value.

12. getoverlap.txt (python?) / badlines.sh (shell)

--> goes through the makemag file to look for large chi/sharp values that will bleed through their alloted spaces from the fortran code. this is not always necessary.

13. stuffprob (fortran) / runstuffprob.sh (shell)

--> uses the make mag file and a log file from allfprep to pull the stellaricity parameters from sextractor (flag, prob) and input into an output catalog

14. Aperture Corrections; preapcor.sh (shell), apcor.sh (shell), daogrow (fortran), mkdel (fortran), <another>.cl (IRAF)

--> make an image with the PSF neighbor stars subtracted; does aperture photometry of the PSF stars using concentric magnitudes; solves the curve of growth for the infinite aperture correction; organizes the outputs; finds a final value;

15. Photometric Standard Stars (many codes)

MODIFICATION: WE have photometric zero points in the header. NO tests for color corrections ... (yet). 

16. magma.pro or magma.rlb.pro (IDL)

--> Applies the photometric transformation equations (15), applies the aperture correction (14), iteratively solves for the color and magnitude of the star using these inputs for a self consistent solution. Combines multiple exposures of the same star (if input) and has the option to bootstrap non-photometric fields to photometric fields. 

17. astrometry (Shell/IRAF)

--> use headers to convert xy coordinates to ra/dec

MODIFICATIONS: Will need a new code. The image headers are already updated using the 2MASS. 

18. mkchartform.pro (IDL)

--> does some book-keeping and pulls extinction from Schelgal maps; does conversion from E(B-V) to the proper colors/magnitudes; computes a radial distance from the dSph/M31. 


