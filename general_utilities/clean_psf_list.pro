pro clean_psf_list,als_file,lst_file,out_lst_file

 ; -- 1. Read & Clean the ALS file
 in = read_als(als_file,als_hdr)
  gd = where(in.magerr lt 0.05 and in.chi-1.5 lt 0.3 and abs(in.sharp) lt 0.3)
  ingd = in(gd)
  ngd = n_elements(ingd.x)

 ; -- 2. Read the LST file
 psf = read_lst(lst_file,lst_hdr)
 
 ; -- 3. Now, build a clean LST subset
 ;    First, we must match the two.
 ;    ====================================
 ;    In theory, we cna match using the IDs, but just in case we need to 
 ;    match on the (x,y) position. This way we could use an ALS file from
 ;    a different run of the photometry. 
 ; 
  match = fltarr(ngd)-1 
 for g = 0, ngd-1 do begin
   sep =  sqrt(((ingd[g].x-psf.x)^2+(ingd[g].y-psf.y)^2)) 
   m_ind = where(sep lt 0.25) 
   if n_elements(m_ind) eq 1 and m_ind[0] ne -1 then begin 
    match[g] = m_ind[0]
   end	   
 endfor 
  ; -- only keep the objects that have a match
  ; -- these are "good" 
  match = match(where(match ne -1))
  ; -- now restrict our psf stars to those in the good als list
  psf = psf[match]

 ; -- 
  get_lun,lun
 openw,lun,out_lst_file
 
 ; -- v0 technique was to write a grep script 
 ;  for i=0, ngd-1 do begin
 ;  str_id = string(cgd[i].id) 
 ;  str_id = strtrim(str_id,2)
 ;  printf,lun,"grep ' "+str_id+" ' v_sky.lst  >> v_sky.lst2"
 ; endfor

 ; -- v1 technique
   ; -- print the header
   printf,lun,lst_hdr.line1
   printf,lun,lst_hdr.line2
   printf,lun,'   '
   ; --    
  for p = 0, n_elements(psf.x)-1 do begin
    printf,lun,psf[p].str_line
  endfor

 close,lun
 free_lun,lun

end
