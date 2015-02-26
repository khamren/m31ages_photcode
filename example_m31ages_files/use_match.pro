PRO match,calib=calib,tol=tol

 ; -- A quick way to match the nir and opt catalogs 

 opt_cat = 'n147.RICNgood' ;'and10.prob'
 nir_cat = 'n147_jk.out'

  print,'Reading File 1' 
   opt_fmt='l,f,f,f,f,f,f,f,f,f,f,f,f'
   readcol,opt_cat,id,ra_hr,ra_min,ra_sec,dec_hr,dec_min,dec_sec,i,ierr,ri,rierr,ct,cterr,format=opt_fmt,/silent
   no_opt = size(id)

  r = (i+(ri))
  rerr = sqrt(rierr^2-ierr^2)
 
  ra =  15.*(ra_hr+ra_min/60.+ra_sec/3600.)
  dec = (dec_hr+dec_min/60.+dec_sec/3600.)


  print,'Reading File 2'
  nit_fmt = 'l,d,d,f,f,f,f,f,f'
  readcol,nir_cat,nir_id1,nir_ra,nir_dec,jmag,jmagerr,kmag,kmagerr,nir_chi,nir_sharp,format=nir_opt,/silent
  no_nir = size(nir_id1)


 if keyword_set(tol) eq 0 then tol=0.5

   ; -- reasonably quick procedure from SDSS-IDL
  print,'Matching.....'
  ntot= djs_angle_match(ra,dec,nir_ra,nir_dec,dtheta=(tol/3600.), mcount=nmatch, mindx=match_ind, mdist=match_dist)
  print,'Done with Matching with '+string(ntot)+' matches.'
 
  match_ind = long(match_ind)

  star = long(0)
  print,'Writing File ...........'
  get_lun,lun
  openw,lun,'match_jk.out'
 

 star =long(where(match_ind ne -1))
 in=long(0)
 while in lt long(ntot) do begin 

   oflt = '(f8.3)'
   odbl = '(d18.9)'
 
   oind = star[in]
   nind = match_ind[star[in]]
 
   id_str   = string(id[oind])
   ra_str   = string(ra[oind],format=odbl)
   dec_str  = string(dec[oind],format=odbl)
  ; --
   r_str    = string(r[oind],format=oflt)
   rerr_str = string(rerr[oind],format=oflt)
   i_str    = string(i[oind],format=oflt)
   ierr_str = string(ierr[oind],format=oflt)
   ct_str    = string(ct[oind],format=oflt)
   cterr_str = string(cterr[oind],format=oflt)
   jmag_str    = string(jmag[nind],format=oflt)
   jmagerr_str = string(jmagerr[nind],format=oflt)
   kmag_str    = string(kmag[nind],format=oflt)
   kmagerr_str = string(kmagerr[nind],format=oflt)                    
  ; --
   chi_str  = string(nir_chi[nind],format=oflt)
   shp_str  = string(nir_sharp[nind],format=oflt)

   printf,lun,id_str+ra_str+dec_str+r_str+rerr_str+i_str+ierr_str+ct_str+cterr_str+jmag_str+jmagerr_str+kmag_str+kmagerr_str+chi_str+shp_str
  in = in+1
 endwhile
 close,lun
 print,'Done.'
END
