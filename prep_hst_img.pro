pro prep_hst_img,input
; --
; -- short script like code to convert input hst drizzle products into their proper units for daophot --
; 
; -- v0.0 -- RLBeaton, 28 Jan 2015
; -- v0.1 -- RLBeaton, 03 Feb 2015
;       * resolved issue with data formatting such that the image can go directly into daophot from this script
;       * putting information from the SCI extension header into the output header file for daophot so no information is lost
;       * adding a history comment with date/time
; -- 

; -- copied from mkopt by D. Nidever -- 
 ; A list file was input
 if strmid(input[0],0,1) eq '@' then begin
  inp = strmid(input[0],1)

   ; Loading the files
   readcol,inp,files,format='A',/silent
   nfiles = n_elements(files)
  endif else begin

   ; Probably an array of filenames
   if n_elements(input) gt 1 then begin
     files = input
     nfiles = n_elements(files)
      ; A globbed list
   endif else begin
     files = file_search(input)
     nfiles = n_elements(files)
   endelse
 endelse

  ; -- start loop
 for i=0, nfiles-1 do begin

  ; -- 0 -- test for file
   dum = file_search(files(i))
  if dum eq '' then begin
   print,files(i),' DOES NOT EXIST'
  end

   ; -- 
  thisfile = files(i)
   ; -- strip ".fits"
  file = strtrim(file_basename(thisfile,'.fits'),2)

  ; -- 1 -- read in header in ext0, 
  ;         read in image in SCI,
  ;         read in image header in SCI

   ; -- EXT0 header
  head0 = headfits(thisfile)
   ; -- SCI header
  jnkdata  = mrdfits(thisfile,'SCI',headsci)
  data = readfits(thisfile,jnkhead,EXTEN_NO='1')
   ;help,data

  ; -- 2.1 -- pull easy header parameters 
  ncomb  = sxpar(headsci,'NCOMBINE')
  skymed = sxpar(headsci,'MDRIZSKY')
  totexp = sxpar(head0,'EXPTIME')
  
  ; -- 2.2 -- pull exptimes 
  exps = fltarr(ncomb)
  for j = 0, ncomb-1 do begin
   no = string(j,format='(i03)')
   tempkey = 'D'+no+'DEXP'
   exps[j] = sxpar(head0,tempkey)
  endfor
  meanexp = mean(exps)
 
  ; -- 3 -- image manipulations
  sky_cps = skymed/meanexp

   print,'Sky for ',thisfile,' in cps ',sky_cps
   print,'Total Exposure for ',thisfile,' in seconds ',totexp
  data = data + sky_cps
  data = data * totexp
   ;help,data

  ; -- 4 -- write out
  ;mwrfits,data,file+'_dao.fits',head0,/create,/ascii
   ; -- reset header parameter EXTEND to 'false' because there are no extensions
  sxaddpar,head0,'EXTEND','F'
   ; -- add in sci head details that are important
  sxaddpar,head0,'NCOMBINE',ncomb,'number of image sets combined during CR rejecti'
  sxaddpar,head0,'MDRIZSKY',skymed,'Sky value computed by AstroDrizzle'
  sxaddpar,head0,'WCSNAME',sxpar(headsci,'WCSNAME')
  ; -- add in calculated sky cps
  sxaddpar,head0,'MSKYCPS',sky_cps,'Sky value in CPS added to image'
   ; -- add in history statement
  sxaddhist,'Converted from drizzle output into counts with sky for daophot on '+systime(),head0
  ; -- typecast data to 32 bit since this is what DAOPHOT wants
  fits_write,file+'_dao.fits',float(data),head0
 endfor


end
