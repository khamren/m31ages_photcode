!\rm n147.cat n147.xy n147.radec1 n147.radec n147.temp
fields n147.raw 1-3 lines="4-" > n147.xy
wcsctran n147.xy n147.radec1 j.fits logical world columns="2 3" formats="%15.10g %15.10g"
fields n147.radec1 1-3 lines="4-" > n147.radec
fields n147.raw "2-" lines="4-" > n147.temp
!paste n147.radec n147.temp > n147.cat
