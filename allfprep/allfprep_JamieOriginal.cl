procedure allfprep(image,xoff,yoff,okay)

string  image {prompt="Image name that you want to use as psf"}
real    xoff  {prompt="X montage offset value"}
real    yoff  {prompt="Y montage offset value"}
bool okay  {prompt="Is the SExtractor finding all the stars?"}


begin

        string image1
        real xoff1,yoff1
        bool okay1
 
        image1=image
        xoff1=xoff
        yoff1=yoff
        okay1=okay

#Loop to find all the objects the first time, bright star problems
while(!okay1) {

!sex allf.fits -c default.sex

#set stdimage=imt4096

display ("allf.fits",1, bpmask="BPM", bpdisplay="none", bpcolors="red", overlay="",
ocolors="green", erase=yes, border_erase=no, select_frame=yes, repeat=no,
fill=yes, zscale=yes, contrast=0.25, zrange=yes, zmask="", nsample=1000,
xcenter=0.5, ycenter=0.5, xsize=1., ysize=1., xmag=1., ymag=1., order=0,
z1=INDEF, z2=INDEF, ztrans="linear", lutfile="")

fields ("sex.cat","2,3",quit_if_miss=no, print_file_n=no, >> "tvm")

tvmark (1,"tvm", logfile="", autolog=no, outimage="", deletions="", commands="",
mark="point", radii="20", lengths="3", font="raster", color=260, label=yes,
number=no, nxoffset=0, nyoffset=0, pointsize=3, txsize=1, tolerance=1.5,
interactive=no)

delete ("tvm",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

# check after tvmark to see if it is okay
okay1=okay

   if(!okay1) {
# change stuff in default.sex if it isn't okay
    !vi default.sex
   }
}


imdelete ("allf.imh,sex1s.fits,sex1s.imh,sex2s.imh",yes, verify=no, default_acti=yes)

rfits ("allf.fits","", "allf.imh", make_image=yes, long_header=no, short_header=yes, datatype="",blank=0., scale=yes, oldirafname=no, offset=0)

copy ("sex.cat","sex1.cat", verbose=yes)

head (image1//".alf",nlines=3, > "tmp")  

concatenate ("tmp,sex1.cat","sex1.tmp", out_type="in_type", append=no)

delete ("sex1.cat,sex.cmb,sex.repl,sex1.als,sex1.off,sex2.als,sex2.cat,sex2.off",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)


rename ("sex1.tmp","sex1.cat", field="all")

print("    ", >> "tmp.inp")
print("allf", >> "tmp.inp")
print(image1//".psf", >> "tmp.inp")
print("sex1.cat", >> "tmp.inp")
print("sex1.als", >> "tmp.inp")
print("sex1s", >> "tmp.inp")
!allstar < tmp.inp > allfprep.log

delete ("tmp.inp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

wfits ("sex1s.imh","sex1s.fits", no, 1., 0., fextn="fits", extensions=no, global_hdr=yes,make_image=yes, long_header=no, short_header=yes, bitpix=0, blocking_fac=0,scale=yes, autoscale=yes)

!sex sex1s.fits -c default.sex

copy ("sex.cat","sex2.cat", verbose=yes)

concatenate ("tmp,sex2.cat","sex2.tmp", out_type="in_type", append=no)

rename ("sex2.tmp","sex2.cat", field="all")

#delete ("sex2.cat",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

print("offset", >> "tmp.inp")
print("sex2.cat", >> "tmp.inp")
print("100000,0,0,0", >> "tmp.inp")
print("sex2.off", >> "tmp.inp")
print("append", >> "tmp.inp")
print("sex1.als", >> "tmp.inp")
print("sex2.off", >> "tmp.inp")
print("sex.cmb", >> "tmp.inp")
!daophot < tmp.inp >> allfprep.log

delete ("tmp.inp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

print("    ", >> "tmp.inp")
print("allf", >> "tmp.inp")
print(image1//".psf", >> "tmp.inp")
print("sex.cmb", >> "tmp.inp")
print("sex2.als", >> "tmp.inp")
print("sex2s", >> "tmp.inp")
!allstar < tmp.inp >> allfprep.log

delete ("tmp.inp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

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
!daophot < tmp.inp >> allfprep.log

delete ("tmp.inp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)

delete ("tmp",yes, verify=no, default_acti=yes, allversions=yes, subfiles=yes)


end
