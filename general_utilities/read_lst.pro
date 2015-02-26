function read_lst,filename,hdr

  hdr = read_dao_header(filename)

  nhdr = 3
  fmt = 'l,f,f,f,f,f'
 readcol,filename,id,x,y,mag,var1,var2,format=fmt,skipline=nhdr
  n_in =  n_elements(x)

 str = {id:long(-99), x:0.0, y:0.0, mag:0.0, var1:0.0, var2:0.0, str_line:hdr.line2}

 str_arr = replicate(str,n_in)
 str_arr.id  = id
 str_arr.x   = x
 str_arr.y   = y
 str_arr.mag  = mag 
 str_arr.var1 = var1
 str_arr.var2 = var2

 ; -- 
 openr,lun,filename,/get_lun
  temp_line = ''

  ; -- read and forget the headers
 for h = 0, nhdr-1 do begin
  readf,lun,temp_line
 endfor
  ; -- put into my structure
 for n = 0, n_in-1 do begin
  readf,lun,temp_line
  str_arr[n].str_line = temp_line
 endfor 
 
 close,lun
 free_lun,lun

 return,str_arr

end
