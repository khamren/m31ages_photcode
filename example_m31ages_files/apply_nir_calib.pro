PRO apply_nir_calib

 ; -- A quick way to match the nir and opt catalogs 

 nir_cat = 'n147.cat'

  nit_fmt = 'l,d,d,f,f,f,f,f,f,f,f'
  readcol,nir_cat,nir_id1,nir_ra,nir_dec,nir_x,nir_y,jmag,jmagerr,kmag,kmagerr,nir_chi,nir_sharp,format=nir_opt,/silent
  ntot = n_elements(nir_id1)

 if keyword_set(calib) eq 0 then begin
   kmag = kmag - 2.35
   kmagerr = sqrt(kmagerr^2+0.003^2)
   jmag = jmag - 2.42
   jmagerr = sqrt(jmagerr^2+0.003^2)
 endif

  print,'Writing File ...........'
  get_lun,lun
  openw,lun,'n147_jk.out'
 

 i=long(0) 
 while i lt ntot do begin 
   oflt = '(f8.3)'
   odbl = '(d18.9)'
 
   nind = i
 
   id_str   = string(nir_id1[nind],format='(i10)')
   ra_str   = string(nir_ra[nind],format=odbl)
   dec_str  = string(nir_dec[nind],format=odbl)
  ; --
   jmag_str    = string(jmag[nind],format=oflt)
   jmagerr_str = string(jmagerr[nind],format=oflt)
   kmag_str    = string(kmag[nind],format=oflt)
   kmagerr_str = string(kmagerr[nind],format=oflt)                    
  ; --
   chi_str  = string(nir_chi[nind],format=oflt)
   shp_str  = string(nir_sharp[nind],format=oflt)

   printf,lun,id_str+ra_str+dec_str+jmag_str+jmagerr_str+kmag_str+kmagerr_str+chi_str+shp_str
  i = i+1
 endwhile
 close,lun
 print,'Done.'
END
