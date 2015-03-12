# allfprep.cl  

#  This IRAF task will run sextractor to find stars in an image, then use allstar
# to subtract these stars from the image, run sextractor again and finally run allstar.
#  It will ID-Offset the 2nd list and then add these two star list together.

#  Finally, it will use the offset between the super-stacked image (here allf.fits)
# and the Reference Image (M by convention) to transform the image coordinates 
# to those of the Reference Image (for use in ALLFRAME).

# The program assumes the following defaults:
#  All SExtractor input files are named default.??? and are in this directory.
#  The image is named allf.fits.
#  X & Y offsets between the super-stacked image and reference image are supplied.
#  A PSF is supplied for the super-stacked image.

# This code was written by Jamie Osthiemer circa 2000 and decyphered by
# K Gilbert and R Beaton in 2007. Some edits were made based on updates of
# daophot & allstar. Comments were added to make this code less opague.
#  06/13/2007

 # -- declaration line of the iraf task
procedure allfprep(image,xoff,yoff,okay)

 # -- set the required input aparemters from the task epar window
string  image {prompt="Image name that you want to use as psf"}
 #
 # -- montage is an old school image alignment code
 # -- here, we just need to know the x and y ofsets of the co-added frame
 # --  from whatever the "master" frame is for the photometry
 # 
real    xoff  {prompt="X montage offset value"}
real    yoff  {prompt="Y montage offset value"}
 # -- the code will interact with you (totally not necessary) and ask you if its done a good job
 # -- I would remove this section
bool okay  {prompt="Is the SExtractor finding all the stars?"}


begin

# -- declare all of our variables because this is a C program (Effecively)
        string image1
        real xoff1,yoff1
        bool okay1
 
        image1=image
        xoff1=xoff
        yoff1=yoff
        okay1=okay

#Loop to find all the objects the first time, bright star problems
while(!okay1) {

# Initial run of SExtractor -- output sex.cat
# -- running SExtractor from the command line on the file allf.fits
# --  using the configuration file default.sex in this directory
# --- it relies on additional files for SExtractor (default.conv, default.nnw and default.param)
# -- default.conv  = convolution kernal, here a 4x4 pixel Gaussian-y thing
#    default.nnw   = neural network weights to separate stars/galaxies
#    default.param = parameters to output to file, here set up to mimic daophot
#    default.sex   = input parameters to setup the star finding and calculations
#
!sex allf.fits -c default.sex

# -- this would make the image display better
#set stdimage=imt4096

# Display image allf.fits 
# **I would not do this part** 
# -- display the image in whatever image display is open
display ("allf.fits",1, bpmask="BPM", bpdisplay="none", bpcolors="red", overlay="", ocolors="green", erase=yes, border_erase=no, select_frame=yes, repeat=no, fill=yes, zscale=yes, contrast=0.25, zrange=yes, zmask="", nsample=1000, xcenter=0.5, ycenter=0.5, xsize=1., ysize=1., xmag=1., ymag=1., order=0, z1=INDEF, z2=INDEF, ztrans="linear", lutfile="") 

# Edit SExtractor output file to contain only X, Y for use in TVMARK
fields ("sex.cat","2,3",quit_if_miss=no, print_file_n=no, >> "tvm")

# Use X,Y from SExtractor in tv-mark to mark detected stars in 
#  already displayed image.
tvmark (1,"tvm", logfile="", autolog=no, outimage="", deletions="", commands="", mark="point", radii="20", lengths="3", font="raster", color=260, label=yes, number=no, nxoffset=0, nyoffset=0, pointsize=3, txsize=1, tolerance=1.5, interactive=no)

# Delete tvmark coordinate file
delete ("tvm",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

# Check with user after tvmark to see if star detection is good.
# Prompt user to enter "okay" parameter from original window.
okay1=okay

   if(!okay1) {
# Change stuff in default.sex to change star finding algorithms (i.e. FWHM, Threshold etc).
    !vi default.sex
   }
}

# Old dao-package required imh style images, we'll use fits
# RLB -- 06/13/2007
#imdelete ("allf.imh,sex1s.fits,sex1s.imh,sex2s.imh",yes, verify=no, default_acti=yes)
imdelete ("sex1s.fits,sex1s.fits,sex2s.fits",yes, verify=no, default_acti=yes)

# originally a conversion was required from fits to imh.
# RLB -- 06/13/2007
#rfits ("allf.fits","", "allf.imh", make_image=yes, long_header=no, short_header=yes, datatype="",blank=0., scale=yes, oldirafname=no, offset=0)

# copying the SExtractor output catalog (sex.cat) to another file 
copy ("sex.cat","sex1.cat", verbose=yes)

# copying the stupid daophot header into the new file
head (image1//".als",nlines=3, > "tmp")  

# appending the header file (tmp) to the top of the sex1.cat file
concatenate ("tmp,sex1.cat","sex1.tmp", out_type="in_type", append=no)

# cleaning up temporary files
delete ("sex1.cat,sex.cmb,sex.repl,sex1.als,sex1.off,sex2.als,sex2.cat,sex2.off",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

# uh, renaming some files for some reason
rename ("sex1.tmp","sex1.cat", field="all")

# Create an input file for Allstar
# Here it is running on allf.fits, with input coorindate sex1.cat and psf by input-image.psf
#  Output: sex1.als & sex1s.fits
# -- The key bit is that ALLStar will subtract the stars from the image, so we can
#     run the source finding again.
#
print("    ", >> "tmp.inp")
print("allf", >> "tmp.inp")
print(image1//".psf", >> "tmp.inp")
print("sex1.cat", >> "tmp.inp")
print("sex1.als", >> "tmp.inp")
print("sex1s", >> "tmp.inp")
!allstar < tmp.inp > allfprep.log

# Delete allstar input file
delete ("tmp.inp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

# Originally to convert imh output from allstar to fits input for SExtractor, no longer required
# RLB -- 06/13/2007
#wfits ("sex1s.imh","sex1s.fits", no, 1., 0., fextn="fits", extensions=no, global_hdr=yes,make_image=yes, long_header=no, short_header=yes, bitpix=0, blocking_fac=0,scale=yes, autoscale=yes)

# Run SExtractor on star subtracted image from Allstar
#  using identical parameters as before
!sex sex1s.fits -c default.sex
# as before, copy over the files
copy ("sex.cat","sex2.cat", verbose=yes)

# -- again append the stupid daophot header to the top of the sextractor output
concatenate ("tmp,sex2.cat","sex2.tmp", out_type="in_type", append=no)

# -- clean up
delete ("sex2.cat",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)
# -- rename
rename ("sex2.tmp","sex2.cat", field="all")

# Make input file for daophot.
# Run offset in daophot to offset the star ID for those stars found in the star-subtracted image.
# Append new list to old list for master star list sex.cmb 
print("offset", >> "tmp.inp")
print("sex2.cat", >> "tmp.inp")
print("100000,0,0,0", >> "tmp.inp")
print("sex2.off", >> "tmp.inp")
print("append", >> "tmp.inp")
print("sex1.als", >> "tmp.inp")
print("sex2.off", >> "tmp.inp")
print("sex.cmb", >> "tmp.inp")

# run the commands in daophot, write to a log file
!daophot < tmp.inp >> allfprep.log

# Delete daophot input file
delete ("tmp.inp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

# Make input file for allstar.
# Use image allf.fits, combined star list (sex.cmb) and output
# sex2.als & sex2s.fits
print("    ", >> "tmp.inp")
print("allf", >> "tmp.inp")
print(image1//".psf", >> "tmp.inp")
print("sex.cmb", >> "tmp.inp")
print("sex2.als", >> "tmp.inp")
print("sex2s", >> "tmp.inp")
# run allstar on the catalog from the star subtracted image
#  write results to the log file
!allstar < tmp.inp >> allfprep.log

# Delete allstar input file
delete ("tmp.inp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

# Make an input file for Daophot.
#  Daophot is used
# First: run offset to apply x,y shifts from super-stacked image to reference image
#  on sex2.als output, makes allf.mag file for allframe input
# Second: rename original catalog sex1.cat, sex1.off
# Third: appends sex1.off to sex2.off
print("offset", >> "tmp.inp")
print("sex2.als", >> "tmp.inp")
print("0,"//xoff1//","//yoff1//",0", >> "tmp.inp")
print("allf.mag", >> "tmp.inp")
print("  ", >> "tmp.inp")
print("offset", >> "tmp.inp")
print("sex1.cat", >> "tmp.inp")
print("0,0,0,0", >> "tmp.inp")
print("sex1.off", >> "tmp.inp")
print("  ", >> "tmp.inp")
print("append", >> "tmp.inp")
print("sex1.off", >> "tmp.inp")
print("sex2.off", >> "tmp.inp")
print("sex.repl", >> "tmp.inp")
print("  ", >> "tmp.inp")

# run daophot to create a master list for running allframe
!daophot < tmp.inp >> allfprep.log

# Deletes daophot input file
delete ("tmp.inp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)
# deletes other files
delete ("tmp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

end
