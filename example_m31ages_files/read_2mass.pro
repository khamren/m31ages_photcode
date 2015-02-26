FUNCTION read_2mass,ecut=ecut

 infile = '2mass.tsv'
  fmt ='d,d,d,d,a,f,f,f,f,f,f,a,a,a,a,a,a'
 readcol,infile,ra1,dec1,ra2,dec2,twomid,jmag,jerr,hmag,herr,kmag,kerr,qflg,rflg,bflg,cflg,xflg,aflg,$
  format=fmt,/silent
 no_in = size(ra1)

  stg = ' '
  flt = -99.99 
 str = { ra:0d, dec:0d, id:stg, $
         j:flt, jerr:flt, h:flt, herr:flt, k:flt, kerr:flt, $
         jk:flt, jkerr:flt, jh:flt, jherr:flt, qflg:'--', cflg:'--'}

 str_arr = replicate(str,no_in[1])

  star = long(0) 
 while star lt no_in[1] do begin
    
   str_arr[star].id = twomid[star]

   str_arr[star].ra = ra1[star]
   str_arr[star].dec = dec1[star]
  
   str_arr[star].j = jmag[star]
   str_arr[star].h = hmag[star]
   str_arr[star].k = kmag[star]

   str_arr[star].jerr = jerr[star]
   str_arr[star].herr = herr[star]
   str_arr[star].kerr = kerr[star]

   str_arr[star].jk = jmag[star]-kmag[star]
   str_arr[star].jh = jmag[star]-hmag[star]
 
   str_arr[star].jkerr = sqrt(jerr[star]*jerr[star] + kerr[star]*kerr[star])
   str_arr[star].jherr = sqrt(jerr[star]*jerr[star] + herr[star]*herr[star])      
 
   str_arr[star].qflg = qflg[star]
   str_arr[star].cflg = cflg[star]

   star = star + 1
 endwhile

 if keyword_set(ecut) then begin
   cut_arr = [0.06,0.06,0.06]
   cut = where(str_arr.jerr lt cut_arr[0] and str_arr.herr lt cut_arr[1] and str_arr.kerr lt cut_arr[2] and str_arr.qflg eq 'AAA' and str_arr.cflg eq '000')
   return,str_arr(cut)
 endif else begin 
   return,str_arr
 endelse

END
