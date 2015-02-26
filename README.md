# m31ages_photcode
Code for the M31AGES Photometry

Our starting point is the SPLASH photometry. The SPLASH photometry was taken with KPNO 4-meter+MOSAIC and each of the 8 chips in MOSAIC were reduced independently. The SPLASH photometry was taken in 3 filters: M (like V), T2 (=I), and DDO51, where the latter is a intermediate band filter inside of M. The images were deep -- 1x 15 min, 2x 15 min and 3x 30 min in M, T2, DDO51 -- and so the sky backgrounds were large (8000 to 10000 counts!). In this limit, DAOPhot did not always perform well. So, I had a piece-wise photometry pipeline broken into small chunks so I could diagnose and resolve problems as they occured. Also, I am not a very good code writer and adopted things from previous graduate students/postdocs.

Here's how the codes should be ordered:

We assume that the images are in units of counts, are split into chips, and are processed. 

1. mkopt.pro (IDL)
--> this generates the OPT files for command line DAOPHOT & ALLSTAR

2. [[CHECK THE NAME]].sh (Shell)
--> copies the OPT file
--> runs basic daophot, exits after a psf list
--> uses goodpsf and lstfilter to edit the psf list removing non-star like sources
--> finishes daophot doing a run through neighbor subtraction
--> runs allstar 

Calls: daophot, allstar, lstfilter, goodpsf

3. 


