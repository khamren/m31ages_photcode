function read_dao_header,infile
; [[TBD]]: Figure out how to optionally return these: line1=line1,line2=line2

 ; -- read in the header to a daophot file
 ;    parses #'s and puts them into a structure (with line1 and line2) for use elsewhere
 ; -- RLBeaton, 05 Feb 2015

 if not keyword_set(infile) then begin
   print,'Please give an input file ...'
   stop 
 endif

  ; -- 
   ; [[TBD]]
   ; -- should test for the file ... 
 openr,lun,infile,/get_lun
  l1 = ''
  l2 = ''
  readf,lun,l1
  readf,lun,l2
  ; -- clean up
 close,lun
 free_lun,lun

 ; -- 
 vars = strsplit(l2,' ',/EXTRACT) 
 if n_elements(vars) ne 10 then begin
  print,'Are you sure that '+infile+' is a daophot file?'
  print,' ... not the correct number of parameters ...'
  stop
 endif


 str = {line1:l1, line2:l2, nl:0, nx:0, ny:0, lowbad:-99., highbad:99999., thresh:0.0, ap1:1.00, phadu:1.00, rdnoise:0.00, frad:0.00}
  ; -- fill the structure
 str.nl = long(vars[0])
 str.nx = long(vars[1])
 str.ny = long(vars[2])
 str.lowbad  = float(vars[3])
 str.highbad = float(vars[4])
 str.thresh  = float(vars[5])
 str.ap1     = float(vars[6])
 str.phadu   = float(vars[7])
 str.rdnoise = float(vars[8])
 str.frad    = float(vars[9])
  ; -- 

 ; -- 
 ;if keyword_set(line1) then line1=l1
 ;if keyword_set(line2) then line2=l2

 return,str

end
