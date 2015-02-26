function read_opt_nir,ecut=ecut,calib=calib
  
 gal_coord = [8.300500,48.508750]
 gal_radius = 7./60.

 infile = 'match_jk.out'
 fmt = 'l,d,d,f,f,f,f,f,f,f,f,f,f,f,f'
 readcol,infile,id,ra,dec,r,rerr,i,ierr,ct,cterr,j,jerr,k,kerr,chi,sharp,format=fmt
 no_in = size(id)

 ; -- apply zero points
 ;if keyword_set(calib) eq 0 then begin
 ;  k = k - 2.30
 ;  kerr = sqrt(kerr^2+0.004^2)
 ;  j = j - 1.64
 ;  jerr = sqrt(jerr^2+0.003^2)
 ;endif

  distance = spheredist(gal_coord[0],gal_coord[1],ra,dec,units=2,theta=posangle)

  jk = j-k
  ij = i-j
  ri = r-i

  ; -- photometric error cuts
 ecut = [0.1,0.1,0.1,0.15,0.15]
  ; -- cut on chi
 ccut = 1.3
  ; -- cut on sharp
 scut = 1.5
  ; -- cut on gprob
 gprob_cut= 0.25
  ; -- define subarry of just "good quality data"

  dbl_def = double(-999.99)
  flt_def = float(-999.00)
 str = {optnir_data, id:0L, ra:dbl_def, dec:dbl_def, j:flt_def, k:flt_def, $
        jerr:flt_def, kerr:flt_def, jk:flt_def, jkerr:flt_def, $
        chi:flt_def, sharp:flt_def, pa:flt_def, radius:flt_def, good:0, $
        r:flt_def, i:flt_def, ct:flt_def, rerr:flt_def, ierr:flt_def, cterr:flt_def, $
        ij:flt_def, ijerr:flt_def, ri:flt_def, rierr:flt_def}

 str_arr = replicate(str,no_in[1])

  star = long(0)
 while star lt no_in[1] do begin
   
    str_arr[star].id = id[star]
    str_arr[star].ra = ra[star]
    str_arr[star].dec = dec[star]

    str_arr[star].j = j[star]
    str_arr[star].k = k[star]
    str_arr[star].jk = jk[star]

    str_arr[star].jerr = jerr[star]
    str_arr[star].kerr = kerr[star]
    str_arr[star].jkerr = sqrt(jerr[star]*jerr[star]+kerr[star]*kerr[star])

    str_arr[star].r = r[star]
    str_arr[star].i = i[star]
    str_arr[star].ct = ct[star]

    str_arr[star].rerr = rerr[star]
    str_arr[star].ierr = ierr[star]
    str_arr[star].cterr = cterr[star]

    str_arr[star].ij = i[star]-j[star]
    str_arr[star].ri = r[star]-i[star]
 
    str_arr[star].ijerr = sqrt(ierr[star]*ierr[star]+jerr[star]*jerr[star])
    str_arr[star].rierr = sqrt(rerr[star]*rerr[star]+ierr[star]*ierr[star])
    
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
  good = where(str_arr.good eq 1)
  str_out = str_arr(good)
  return,str_out
 endif else begin
  return,str_arr
 endelse

END
