PRO mkchartform

posnfile='all.cat'
chartfile='all.dat'
ra_cen = 15.*(00.+47./60.+05.17/3600.)
dec_cen = 34.+26./60.+58.99/3600.

deltam = 2.0
deltat = 1.6
deltad = -0.27 

;+
;
; This procedure was written to make a catalog file to be used in IRAF.MTOOLS.CHART task.
; The input "posnfile" assumes the following format:
;
; ID RA DEC ID X Y Dmag Derr Tmag Terr Mmag Merr CHI SHARP FLAG PROB
;
; Assumed RA input is in DECIMAL DEGREES. This RA is converted to DECIMAL HOURS for computing gl and gb
;
; 'ra_cen' and 'dec_cen' are optional parameters in case you want to include the radial angular 
; distance in arcminumstar from a given point. These parameters are 0 by default if not specified.
;
; FILE OUTPUT: 
; ID X Y RA DEC Dmag Derr Tmag Terr Mmag Merr CHI SHARP flag prob r gl gb E(B-V) E(M-T) E(M-D) Am
;
; NO changes are made to the magnitudes, magnitude errors, chi and sharp. The reddenning and extinction
; values are not applied to the magnitudes. No header line is printed to the file.
;
; Example:
;
; .r mkchartform
; mkchartform,'inputfile.cat','outputfile.dat',10.6846853,41.2690374
;
;-
;if (n_params(0) eq 0) then begin
;   print,'CALLING SEQUENCE : mkchartform,posnfile,chartfile,ra_cen,dec_cen'
;   return
;endif
;
; Read in the columnumstar from the input file.
  readcol,posnfile,id,ra,dec,id1,x,y,d,derr,m,merr,t,terr,chi,sharp,flag,prob,format='l,d,d,l,f,f,f,f,f,f,f,f,f,f,l,f'
  numstar = 0l
  numstar = n_elements(id)
  ;flag = fltarr(numstar)
  ;prob = fltarr(numstar)
  m = m +deltam
  t = t +deltat
  d = d +deltad

;
; Convert decimal degrees to decimal hours
ra1=(ra/15)
;
; Call GLACTC.PRO (ASTRO routine) to convert RA and Dec into Galactic longitude (gl) and latitude(lb).
  glactc,ra1,dec,2000.,gl,gb,1
;
; Call DUST_GETVAL.PRO to get the 
	ebv = dust_getval(gl,gb,/interp,map="Ebv",ipath='/net/grass/catalogs/reddening/maps/',/noloop)
;
; Compute E(M-T2), E(M-DDO51), and A(M) based on E(B-V) 
  emt = 1.60*ebv
  emd = 0.06*ebv
  am  = 3.43*ebv
;
; Calculate the radial angular distances.
  r = sqrt(((ra-ra_cen)*cos(dec_cen)*15.)^2 + (dec-dec_cen)^2)*60.
;
; Output to the chartfile.
  fmt = '(i8,2f11.4,2f17.9,8f11.4,i3,f11.4,f13.3,2f12.3,4f11.4)'
  get_lun,unit
  openw,unit,chartfile
     for i = 0l, numstar-1 do begin
        printf,unit,format=fmt,id[i],x[i],y[i],ra[i],dec[i],d[i],derr[i],m[i],merr[i],t[i],terr[i],chi[i],sharp[i]$
		,flag[i],prob[i],r[i],gl[i],gb[i],ebv[i],emt[i],emd[i],am[i]
     endfor
  close,unit
  free_lun,unit
;
END

