function read_nir,ecut=ecut,calib=calib
  
 gal_coord = [8.300500,48.508750]

 infile = 'n147.cat'
 fmt = 'l,d,d,f,f,f,f,f,f,f,f'
 readcol,infile,id,ra,dec,x,y,j,jerr,k,kerr,chi,sharp,format=fmt
 no_in = size(id)

;if keyword_set(calib) eq 0 then begin
;  k = k - 2.30
;   kerr = sqrt(kerr^2+0.003^2)
;  j = j - 1.64
;   jerr = sqrt(jerr^2+0.002^2)
;endif

  jk = j-k

  distance = spheredist(gal_coord[0],gal_coord[1],ra,dec,units=2,theta=posangle)

  ; -- photometric error cuts
 ecut = [0.1,0.1,0.1,0.10,0.10]
  ; -- cut on chi
 ccut = 0.7
  ; -- cut on sharp
 scut = 0.7
  ; -- define subarry of just "good quality data"
 gd = where(jerr lt ecut[3] and kerr lt ecut[4] and (chi-1) lt ccut and abs(sharp) lt scut, complement=notgd )

  dbl_def = double(-999.00)
  flt_def = float(-999.00)
 str = {nir_data, id:0L, ra:dbl_def, dec:dbl_def, x:flt_def, y:flt_def, j:flt_def, k:flt_def, $
        jerr:flt_def, kerr:flt_def, jk:flt_def, jkerr:flt_def, $
        chi:flt_def, sharp:flt_def, pa:flt_def, radius:flt_def, good:0}

 str_arr = replicate(str,no_in[1])

  star = long(0)
 while star lt no_in[1] do begin
   
    str_arr[star].id = id[star]
    str_arr[star].ra = ra[star]
    str_arr[star].dec = dec[star]
    str_arr[star].x  = x[star]
    str_arr[star].y  = y[star]

    str_arr[star].j = j[star]
    str_arr[star].k = k[star]
    str_arr[star].jk = jk[star]

    str_arr[star].jerr = jerr[star]
    str_arr[star].kerr = kerr[star]
    str_arr[star].jkerr = sqrt(jerr[star]*jerr[star]+kerr[star]*kerr[star])

    str_arr[star].chi = chi[star]
    str_arr[star].sharp = sharp[star]

    str_arr[star].radius = distance[star]
    str_arr[star].pa = posangle[star]
    if jerr[star] lt ecut[3] and kerr[star] lt ecut[4] and (chi[star]-1) lt ccut and abs(sharp[star]) lt scut then begin
     str_arr[star].good = 1
    endif


  star = star + 1
 endwhile

 if keyword_set(ecut) then begin
  good = where(str_arr.jerr lt ecut[3] and str_arr.kerr lt ecut[4] and $
               str_arr.chi-1 lt ccut and abs(str_arr.sharp) lt scut)
  str_out = str_arr(good)
  return,str_out
 endif else begin
  return,str_arr
 endelse

END
