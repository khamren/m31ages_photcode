# Remove Files
!\rm chip?.xy
!\rm chip?.radec
!\rm chip?.radec1
!\rm chip?.cat
!\rm all.cat

# Cp Cols 1-3 (ID X Y) into a separate file
fields chip1.stuffprob 1-3 lines="-" > chip1.xy
fields chip2.stuffprob 1-3 lines="-" > chip2.xy
fields chip3.stuffprob 1-3 lines="-" > chip3.xy
fields chip4.stuffprob 1-3 lines="-" > chip4.xy
fields chip5.stuffprob 1-3 lines="-" > chip5.xy
fields chip6.stuffprob 1-3 lines="-" > chip6.xy
fields chip7.stuffprob 1-3 lines="-" > chip7.xy
fields chip8.stuffprob 1-3 lines="-" > chip8.xy

# Convert .xy to .radec1 files
#        XY file  RA/Dec File  Image with WCS infmt   outfmt col          output format statement
wcsctran chip1.xy chip1.radec1 obj2062_1.fits logical world columns="2 3" formats="%15.10g %15.10g"
wcsctran chip2.xy chip2.radec1 obj2062_2.fits logical world columns="2 3" formats="%15.10g %15.10g"
wcsctran chip3.xy chip3.radec1 obj2062_3.fits logical world columns="2 3" formats="%15.10g %15.10g"
wcsctran chip4.xy chip4.radec1 obj2062_4.fits logical world columns="2 3" formats="%15.10g %15.10g"
wcsctran chip5.xy chip5.radec1 obj2062_5.fits logical world columns="2 3" formats="%15.10g %15.10g"
wcsctran chip6.xy chip6.radec1 obj2062_6.fits logical world columns="2 3" formats="%15.10g %15.10g"
wcsctran chip7.xy chip7.radec1 obj2062_7.fits logical world columns="2 3" formats="%15.10g %15.10g"
wcsctran chip8.xy chip8.radec1 obj2062_8.fits logical world columns="2 3" formats="%15.10g %15.10g"

# Cut out top 3 lines of the WCSCTRAN output (header added by routine)
fields chip1.radec1 1-3 lines="4-" > chip1.radec
fields chip2.radec1 1-3 lines="4-" > chip2.radec
fields chip3.radec1 1-3 lines="4-" > chip3.radec
fields chip4.radec1 1-3 lines="4-" > chip4.radec
fields chip5.radec1 1-3 lines="4-" > chip5.radec
fields chip6.radec1 1-3 lines="4-" > chip6.radec
fields chip7.radec1 1-3 lines="4-" > chip7.radec
fields chip8.radec1 1-3 lines="4-" > chip8.radec

# Put together the original photometry file & the new RA/Dec
!paste chip1.radec chip1.stuffprob > chip1.cat
!paste chip2.radec chip2.stuffprob > chip2.cat
!paste chip3.radec chip3.stuffprob > chip3.cat
!paste chip4.radec chip4.stuffprob > chip4.cat
!paste chip5.radec chip5.stuffprob > chip5.cat
!paste chip6.radec chip6.stuffprob > chip6.cat
!paste chip7.radec chip7.stuffprob > chip7.cat
!paste chip8.radec chip8.stuffprob > chip8.cat

# Remove intermediate files
!\rm chip?.radec1

# Merge Chips For Eval
!cat chip?.cat > all.cat
